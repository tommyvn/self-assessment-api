package uk.gov.hmrc.selfassessmentapi.repositories.domain

import org.joda.time.DateTime
import uk.gov.hmrc.domain.SaUtr
import uk.gov.hmrc.selfassessmentapi.{CapAt, _}
import uk.gov.hmrc.selfassessmentapi.domain.selfemployment.ExpenseType
import uk.gov.hmrc.selfassessmentapi.domain.unearnedincome.SavingsIncomeType
import uk.gov.hmrc.selfassessmentapi.domain._
import uk.gov.hmrc.selfassessmentapi.services.live.calculation.steps.DividendsFromUKSourcesCalculation._
import uk.gov.hmrc.selfassessmentapi.services.live.calculation.steps.SelfAssessment

object Deductions {
  def apply(selfAssessment: SelfAssessment) = {
    new Deductions(incomeTaxRelief = RoundUp(selfAssessment.selfEmployments.map(LossBroughtForward(_)).sum),
      totalDeductions = PersonalAllowance(selfAssessment.selfEmployments))
  }
}

object DividendsFromUKSources {
  def apply(selfAssessment: SelfAssessment) = selfAssessment.unearnedIncomes.map { unearnedIncome =>
    new DividendsFromUKSources(unearnedIncome.sourceId, RoundDown(unearnedIncome.dividends.map(_.amount).sum))
  }
}
object InterestFromUKBanksAndBuildingSocieties {
  def apply(selfAssessment: SelfAssessment) =
    selfAssessment.unearnedIncomes.map { income =>
      new InterestFromUKBanksAndBuildingSocieties(income.sourceId, income.savings.map {saving =>
        saving.`type` match {
          case SavingsIncomeType.InterestFromBanksTaxed => saving.amount * 100 / 80
          case SavingsIncomeType.InterestFromBanksUntaxed => saving.amount
        }
      }.sum)
    }
}

object TotalIncomeOnWhichTaxIsDue {
  def apply(selfEmployments: SelfAssessment) = PositiveOrZero(TotalIncomeReceived(selfEmployments) - TotalAllowancesAndReliefs(selfEmployments.selfEmployments))
}

object TotalAllowancesAndReliefs {
  def apply(selfEmployments: Seq[MongoSelfEmployment]) = RoundUp(selfEmployments.map(LossBroughtForward(_)).sum) +
    PersonalAllowance(selfEmployments)
}

object PersonalAllowance {
  private val standardAllowance = BigDecimal(11000)
  private val taperingThreshold = BigDecimal(100000)

  def apply(selfEmployments: Seq[MongoSelfEmployment]) = TotalTaxableIncome(selfEmployments) - TotalTaxableIncome(selfEmployments) % 2
  match {
    case income if income <= taperingThreshold => standardAllowance
    case income if income > taperingThreshold => PositiveOrZero(standardAllowance - ((income - taperingThreshold) / 2))
  }
}

object TotalTaxableIncome {
  def apply(selfEmployments: Seq[MongoSelfEmployment]) = selfEmployments.map(selfEmployment => TaxableProfit(selfEmployment)).sum
}

object TotalIncomeReceived {
  def apply(selfAssessment: SelfAssessment) = {
    selfAssessment.selfEmployments.map(selfEmployment => NetProfit(selfEmployment, TaxableProfit(selfEmployment))).sum +
    InterestFromUKBanksAndBuildingSocieties(selfAssessment).map(_.totalInterest).sum +
    DividendsFromUKSources(selfAssessment).map(_.totalDividend).sum
  }
}

object Profit {

  private val annualInvestmentAllowanceThreshold = 200000

  def apply(selfEmployment: MongoSelfEmployment) = {
    val profitIncreases = {
      val adjustments = selfEmployment.adjustments.map(a => Sum(a.basisAdjustment, a.accountingAdjustment, a.averagingAdjustment))
      Sum(selfEmployment.incomes) + Sum(selfEmployment.balancingCharges) + Sum(selfEmployment.goodsAndServicesOwnUse) +
        adjustments.getOrElse(0)
    }

    val profitReductions = {
      val adjustments = selfEmployment.adjustments.map { a => Sum(a.includedNonTaxableProfits, a.overlapReliefUsed) }
      Sum(selfEmployment.expenses.filterNot(_.`type` == ExpenseType.Depreciation)) +
        CapAt(selfEmployment.allowances.map(_.total), annualInvestmentAllowanceThreshold).get + adjustments.get
    }

    PositiveOrZero(profitIncreases - profitReductions)
  }
}

object TaxableProfit {
  def apply(selfEmployment: MongoSelfEmployment) = {
    Profit(selfEmployment) - LossBroughtForward(selfEmployment) + selfEmployment.outstandingBusinessIncome
  }
}

object LossBroughtForward {
  def apply(selfEmployment: MongoSelfEmployment) = ValueOrZero(CapAt(selfEmployment.adjustments.flatMap(_.lossBroughtForward), Profit
  (selfEmployment)))
}

object NetProfit {
  def apply(selfEmployment: MongoSelfEmployment, taxableProfit: BigDecimal) = taxableProfit + LossBroughtForward(selfEmployment)
}

object SelfEmploymentProfits {
  def apply(selfEmployments: Seq[MongoSelfEmployment]) = new SelfEmploymentProfits(selfEmployments.map {
    selfEmployment => SelfEmploymentProfit(selfEmployment.sourceId, RoundDown(TaxableProfit(selfEmployment)),
      RoundDown(NetProfit(selfEmployment, TaxableProfit(selfEmployment))), LossBroughtForward(selfEmployment))
  })
}

class FunctionalLiability(saUtr: SaUtr, taxYear: TaxYear, selfEmploymentProfits: SelfEmploymentProfits, totalIncomeReceived: BigDecimal,
                          totalTaxableIncome: BigDecimal, personalAllowance: BigDecimal, totalAllowancesAndReliefs: BigDecimal,
                          totalIncomeOnWhichTaxIsDue: BigDecimal,
                          interestFromUKBanksAndBuildingSocieties: Seq[InterestFromUKBanksAndBuildingSocieties],
                          dividendsFromUKSources: Seq[DividendsFromUKSources],
                          deductions: Deductions)

object FunctionalLiability {

  def apply(saUtr: SaUtr, taxYear: TaxYear, selfAssessment: SelfAssessment, createdDateTime: DateTime = DateTime.now()):
  FunctionalLiability =

    new FunctionalLiability(saUtr, taxYear, SelfEmploymentProfits(selfAssessment.selfEmployments),
      TotalIncomeReceived(selfAssessment),
      TotalTaxableIncome(selfAssessment.selfEmployments), PersonalAllowance(selfAssessment.selfEmployments),
      totalAllowancesAndReliefs = TotalAllowancesAndReliefs(selfAssessment.selfEmployments),
      totalIncomeOnWhichTaxIsDue = TotalIncomeOnWhichTaxIsDue(selfAssessment),
      interestFromUKBanksAndBuildingSocieties = InterestFromUKBanksAndBuildingSocieties(selfAssessment),
      dividendsFromUKSources = DividendsFromUKSources(selfAssessment),
      deductions = Deductions(selfAssessment))

}

class SelfEmploymentProfits(profits: Seq[SelfEmploymentProfit])

case class SelfEmploymentProfit(sourceId: SourceId, totalProfit: BigDecimal, netProfit: BigDecimal, lossBroughtForward: BigDecimal)


