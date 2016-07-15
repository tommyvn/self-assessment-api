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
import uk.gov.hmrc.selfassessmentapi.services.live.calculation.steps.SelfAssessment
import uk.gov.hmrc.selfassessmentapi.{CapAt, _}

object IncomeTaxRelief {
  def apply(selfAssessment: SelfAssessment) = RoundUp(selfAssessment.selfEmployments.map(LossBroughtForward(_)).sum)
}

object Deductions {
  def apply(selfAssessment: SelfAssessment) = {
    new Deductions(incomeTaxRelief = IncomeTaxRelief(selfAssessment), totalDeductions = TotalDeduction(selfAssessment))
  }
}

object DividendsFromUKSources {
  def apply(selfAssessment: SelfAssessment) = selfAssessment.unearnedIncomes.map { unearnedIncome =>
    new DividendsFromUKSources(unearnedIncome.sourceId, RoundDown(unearnedIncome.dividends.map(_.amount).sum))
  }
}

object InterestFromUKBanksAndBuildingSocieties {
  def apply(selfAssessment: SelfAssessment): Seq[InterestFromUKBanksAndBuildingSocieties] =
    selfAssessment.unearnedIncomes.map { income =>
      new InterestFromUKBanksAndBuildingSocieties(income.sourceId, RoundDown(income.savings.map {saving =>
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
  def apply(selfAssessment: SelfAssessment) = selfAssessment.selfEmployments.map(selfEmployment => TaxableProfitFromSelfEmployment(selfEmployment)).sum
}

object TotalIncomeReceived {
  def apply(selfAssessment: SelfAssessment): BigDecimal = {
    selfAssessment.selfEmployments.map(selfEmployment => ProfitFromSelfEmployment(selfEmployment)).sum +
    InterestFromUKBanksAndBuildingSocieties(selfAssessment).map(_.totalInterest).sum +
    DividendsFromUKSources(selfAssessment).map(_.totalDividend).sum
  }

  def apply(selfAssessment: SelfAssessment, interestFromUKBanksAndBuildingSocieties: Seq[InterestFromUKBanksAndBuildingSocieties],
            dividendsFromUKSources: Seq[DividendsFromUKSources]): BigDecimal = {
    selfAssessment.selfEmployments.map(selfEmployment => ProfitFromSelfEmployment(selfEmployment)).sum +
      interestFromUKBanksAndBuildingSocieties.map(_.totalInterest).sum +
      dividendsFromUKSources.map(_.totalDividend).sum
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
    AdjustedProfits(selfEmployment) - LossBroughtForward(selfEmployment) + selfEmployment.outstandingBusinessIncome
  }
}

object ProfitFromSelfEmployment {
  def apply(selfEmployment: MongoSelfEmployment) : BigDecimal = apply(TaxableProfitFromSelfEmployment(selfEmployment), LossBroughtForward(selfEmployment))
  def apply(taxableProfit: BigDecimal, lossBroughtForward: BigDecimal): BigDecimal = taxableProfit + lossBroughtForward
}


object LossBroughtForward {
  def apply(selfEmployment: MongoSelfEmployment): BigDecimal = apply(selfEmployment, AdjustedProfits(selfEmployment))
  def apply(selfEmployment: MongoSelfEmployment, adjustedProfits: BigDecimal): BigDecimal = ValueOrZero(CapAt(selfEmployment.adjustments.flatMap(_.lossBroughtForward),
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
      deductions = Deductions(selfAssessment))

}

class SelfEmploymentProfits(profits: Seq[SelfEmploymentProfit])

case class SelfEmploymentProfit(sourceId: SourceId, totalProfit: BigDecimal, netProfit: BigDecimal, lossBroughtForward: BigDecimal)
