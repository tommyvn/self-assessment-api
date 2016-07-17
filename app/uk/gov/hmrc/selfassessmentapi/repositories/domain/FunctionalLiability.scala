/*
 * Copyright 2016 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.selfassessmentapi.repositories.domain

import org.joda.time.DateTime
import uk.gov.hmrc.domain.SaUtr
import uk.gov.hmrc.selfassessmentapi.domain.selfemployment.ExpenseType
import uk.gov.hmrc.selfassessmentapi.domain.unearnedincome.SavingsIncomeType
import uk.gov.hmrc.selfassessmentapi.domain.{Deductions, _}
import uk.gov.hmrc.selfassessmentapi.repositories.domain.TaxBand.{AdditionalHigherTaxBand, BasicTaxBand, HigherTaxBand, TaxBandRangeCheck}
import uk.gov.hmrc.selfassessmentapi.services.live.calculation.steps.SelfAssessment
import uk.gov.hmrc.selfassessmentapi.{CapAt, _}

object PersonalDividendAllowance {
  private def remainingPersonalAllowance(personalAllowance: BigDecimal, incomeTaxRelief: BigDecimal, totalProfitFromSelfEmployments: BigDecimal) = {
    personalAllowance + incomeTaxRelief - totalProfitFromSelfEmployments
  }

  private def taxableDividendIncome(totalDividends: BigDecimal, personalAllowance: BigDecimal, incomeTaxRelief: BigDecimal, totalProfitFromSelfEmployments: BigDecimal) = {
    totalDividends - remainingPersonalAllowance(personalAllowance, incomeTaxRelief, totalProfitFromSelfEmployments)
  }

  def apply(implicit selfAssessment: SelfAssessment): BigDecimal = apply(TotalProfitFromSelfEmployments(selfAssessment), IncomeTaxRelief(selfAssessment),
    PersonalAllowance(selfAssessment), TotalSavings(selfAssessment), TotalDividends(selfAssessment))

  def apply(totalProfitFromSelfEmployments: BigDecimal, incomeTaxRelief: BigDecimal, personalAllowance: BigDecimal,
            totalSavings: BigDecimal, totalDividends: BigDecimal): BigDecimal = {
    val taxableDividend =  taxableDividendIncome(totalDividends, personalAllowance, incomeTaxRelief, totalProfitFromSelfEmployments)
    CapAt(totalProfitFromSelfEmployments match {
      case profit if profit < (personalAllowance + incomeTaxRelief) =>
        totalSavings match {
          case saving if saving == 0 => PositiveOrZero(taxableDividend)
          case saving if (profit + saving) < (personalAllowance + incomeTaxRelief) => PositiveOrZero(taxableDividend + saving)
          case _ => totalDividends
        }
      case _ => totalDividends
    }, 5000)
  }

}
object PayPensionProfitsTax {
  private def allocateToTaxBands(income: BigDecimal, taxBands: Seq[TaxBand]): Seq[TaxBandAllocation] = taxBands match {
    case taxBand :: nextBands =>
      Seq(TaxBandAllocation(taxBand.allocate(income), taxBand)) ++ allocateToTaxBands(income - taxBand.allocate(income), nextBands)
    case Nil => Nil
  }

  def apply(selfAssessment: SelfAssessment): Seq[TaxBandAllocation] = {
    apply(TotalProfitFromSelfEmployments(selfAssessment), TotalDeduction(selfAssessment))
  }

  def apply(totalProfitFromSelfEmployments: BigDecimal, totalDeduction: BigDecimal): Seq[TaxBandAllocation] = {
    allocateToTaxBands(PositiveOrZero(totalProfitFromSelfEmployments - totalDeduction), Seq(BasicTaxBand, HigherTaxBand, AdditionalHigherTaxBand))
  }

}

object TotalProfitFromSelfEmployments {
  def apply(selfAssessment: SelfAssessment) = selfAssessment.selfEmployments.map(ProfitFromSelfEmployment(_)).sum
}

object SavingsStartingRate {
  private val startingRateLimit = BigDecimal(5000)
  def apply(selfAssessment: SelfAssessment): BigDecimal = apply(TotalProfitFromSelfEmployments(selfAssessment), TotalDeduction(selfAssessment))
  def apply(profitFromSelfEmployments: BigDecimal, totalDeduction: BigDecimal): BigDecimal = PositiveOrZero(startingRateLimit - PositiveOrZero(profitFromSelfEmployments - totalDeduction))
}

object PersonalSavingsAllowance {
  def apply(selfAssessment: SelfAssessment): BigDecimal = apply(TotalIncomeOnWhichTaxIsDue(selfAssessment))
  def apply(totalIncomeOnWhichTaxIsDue: BigDecimal): BigDecimal = totalIncomeOnWhichTaxIsDue match {
    case total if total < 1 => 0
    case total if total isWithin BasicTaxBand => 1000
    case total if total isWithin HigherTaxBand => 500
    case _ => 0
  }
}

object IncomeTaxRelief {
  def apply(selfAssessment: SelfAssessment) = RoundUp(selfAssessment.selfEmployments.map(LossBroughtForward(_)).sum)
}

object IncomeTaxReliefAndDeductions {
  def apply(selfAssessment: SelfAssessment) = {
    new Deductions(incomeTaxRelief = IncomeTaxRelief(selfAssessment), totalDeductions = TotalDeduction(selfAssessment))
  }
}

object TotalDividends {
  def apply(selfAssessment: SelfAssessment) = DividendsFromUKSources(selfAssessment).map(_.totalDividend).sum
}

object DividendsFromUKSources {
  def apply(selfAssessment: SelfAssessment) = selfAssessment.unearnedIncomes.map { unearnedIncome =>
    new DividendsFromUKSources(unearnedIncome.sourceId, RoundDown(unearnedIncome.dividends.map(_.amount).sum))
  }
}

object TotalSavings {
  def apply(selfAssessment: SelfAssessment) = InterestFromUKBanksAndBuildingSocieties(selfAssessment).map(_.totalInterest).sum
}

object InterestFromUKBanksAndBuildingSocieties {
  def apply(selfAssessment: SelfAssessment): Seq[InterestFromUKBanksAndBuildingSocieties] =
    selfAssessment.unearnedIncomes.map { income =>
      new InterestFromUKBanksAndBuildingSocieties(income.sourceId, RoundDown(income.savings.map { saving =>
        saving.`type` match {
          case SavingsIncomeType.InterestFromBanksTaxed => saving.amount * 100 / 80
          case SavingsIncomeType.InterestFromBanksUntaxed => saving.amount
        }
      }.sum))
    }
}

object TotalIncomeOnWhichTaxIsDue {
  def apply(selfAssessment: SelfAssessment): BigDecimal = apply(TotalIncomeReceived(selfAssessment), TotalDeduction(selfAssessment))

  def apply(totalIncomeReceived: BigDecimal, totalDeduction: BigDecimal): BigDecimal = PositiveOrZero(totalIncomeReceived - totalDeduction)
}

object TotalDeduction {
  def apply(selfAssessment: SelfAssessment): BigDecimal = IncomeTaxRelief(selfAssessment) + PersonalAllowance(selfAssessment)

  def apply(incomeTaxRelief: BigDecimal, personalAllowance: BigDecimal): BigDecimal = incomeTaxRelief + personalAllowance
}

object PersonalAllowance {
  private val standardAllowance = BigDecimal(11000)
  private val taperingThreshold = BigDecimal(100000)

  def apply(selfAssessment: SelfAssessment): BigDecimal = apply(TotalTaxableIncome(selfAssessment))

  def apply(totalTaxableIncome: BigDecimal): BigDecimal = totalTaxableIncome - totalTaxableIncome % 2
  match {
    case income if income <= taperingThreshold => standardAllowance
    case income if income > taperingThreshold => PositiveOrZero(standardAllowance - ((income - taperingThreshold) / 2))
  }
}

object TotalTaxableIncome {
  def apply(selfAssessment: SelfAssessment) = selfAssessment.selfEmployments.map(selfEmployment => TaxableProfitFromSelfEmployment
  (selfEmployment)).sum
}

object TotalIncomeReceived {
  def apply(selfAssessment: SelfAssessment): BigDecimal = {
    apply(TotalProfitFromSelfEmployments(selfAssessment), TotalSavings(selfAssessment), TotalDividends(selfAssessment))
  }

  def apply(totalProfitFromSelfEmployments: BigDecimal, totalSavings: BigDecimal, totalDividends: BigDecimal): BigDecimal = {
    totalProfitFromSelfEmployments + totalSavings + totalDividends
  }

}

object AdjustedProfits {

  private val annualInvestmentAllowanceThreshold = 200000

  def apply(selfEmployment: MongoSelfEmployment) = {
    val profitIncreases = {
      val adjustments = selfEmployment.adjustments.map(a => Sum(a.basisAdjustment, a.accountingAdjustment, a.averagingAdjustment))
      Total(selfEmployment.incomes) + Total(selfEmployment.balancingCharges) + Total(selfEmployment.goodsAndServicesOwnUse) +
        adjustments.getOrElse(0)
    }

    val profitReductions = {
      val adjustments = selfEmployment.adjustments.map { a => Sum(a.includedNonTaxableProfits, a.overlapReliefUsed) }
      Total(selfEmployment.expenses.filterNot(_.`type` == ExpenseType.Depreciation)) +
        CapAt(selfEmployment.allowances.map(_.total), annualInvestmentAllowanceThreshold).getOrElse(0) + adjustments.getOrElse(0)
    }

    PositiveOrZero(profitIncreases - profitReductions)
  }
}

object TaxableProfitFromSelfEmployment {
  def apply(selfEmployment: MongoSelfEmployment) = {
    RoundDown(AdjustedProfits(selfEmployment) - LossBroughtForward(selfEmployment) + selfEmployment.outstandingBusinessIncome)
  }
}

object ProfitFromSelfEmployment {
  def apply(selfEmployment: MongoSelfEmployment): BigDecimal = apply(TaxableProfitFromSelfEmployment(selfEmployment), LossBroughtForward
  (selfEmployment))

  def apply(taxableProfit: BigDecimal, lossBroughtForward: BigDecimal): BigDecimal = RoundDown(taxableProfit + lossBroughtForward)
}


object LossBroughtForward {
  def apply(selfEmployment: MongoSelfEmployment): BigDecimal = apply(selfEmployment, AdjustedProfits(selfEmployment))

  def apply(selfEmployment: MongoSelfEmployment, adjustedProfits: BigDecimal): BigDecimal = ValueOrZero(CapAt(selfEmployment.adjustments
    .flatMap(_.lossBroughtForward),
    adjustedProfits))
}

object SelfEmploymentProfits {
  def apply(selfAssessment: SelfAssessment) = new SelfEmploymentProfits(selfAssessment.selfEmployments.map {
    selfEmployment => SelfEmploymentProfit(selfEmployment.sourceId, RoundDown(TaxableProfitFromSelfEmployment(selfEmployment)),
      RoundDown(ProfitFromSelfEmployment(selfEmployment)), LossBroughtForward(selfEmployment))
  })
}

class FunctionalLiability(saUtr: SaUtr, taxYear: TaxYear, selfEmploymentProfits: SelfEmploymentProfits, totalIncomeReceived: BigDecimal,
                          totalTaxableIncome: BigDecimal, personalAllowance: BigDecimal, totalAllowancesAndReliefs: BigDecimal,
                          totalIncomeOnWhichTaxIsDue: BigDecimal,
                          interestFromUKBanksAndBuildingSocieties: Seq[InterestFromUKBanksAndBuildingSocieties],
                          dividendsFromUKSources: Seq[DividendsFromUKSources],
                          deductions: Deductions)

object FunctionalLiability {

  def apply(saUtr: SaUtr, taxYear: TaxYear, selfAssessment: SelfAssessment, createdDateTime: DateTime = DateTime.now()) =
    new FunctionalLiability(saUtr, taxYear, SelfEmploymentProfits(selfAssessment),
      TotalIncomeReceived(selfAssessment),
      TotalTaxableIncome(selfAssessment), PersonalAllowance(selfAssessment),
      totalAllowancesAndReliefs = TotalDeduction(selfAssessment),
      totalIncomeOnWhichTaxIsDue = TotalIncomeOnWhichTaxIsDue(selfAssessment),
      interestFromUKBanksAndBuildingSocieties = InterestFromUKBanksAndBuildingSocieties(selfAssessment),
      dividendsFromUKSources = DividendsFromUKSources(selfAssessment),
      deductions = IncomeTaxReliefAndDeductions(selfAssessment))

}

class SelfEmploymentProfits(profits: Seq[SelfEmploymentProfit])

case class SelfEmploymentProfit(sourceId: SourceId, totalProfit: BigDecimal, netProfit: BigDecimal, lossBroughtForward: BigDecimal)
