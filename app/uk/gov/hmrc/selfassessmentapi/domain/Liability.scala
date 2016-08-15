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

package uk.gov.hmrc.selfassessmentapi.domain

import play.api.libs.json.Json
import uk.gov.hmrc.selfassessmentapi.config.{AppContext, FeatureSwitch}
import uk.gov.hmrc.selfassessmentapi.repositories.domain.{EmploymentIncome, SelfEmploymentIncome, UkPropertyIncome}

case class InterestFromUKBanksAndBuildingSocieties(sourceId: String, totalInterest: BigDecimal)

object InterestFromUKBanksAndBuildingSocieties {
  implicit val format = Json.format[InterestFromUKBanksAndBuildingSocieties]
}

case class DividendsFromUKSources(sourceId: String, totalDividend: BigDecimal)

object DividendsFromUKSources {
  implicit val format = Json.format[DividendsFromUKSources]
}

case class NonSavingsIncomes(employment: Seq[EmploymentIncome], selfEmployment: Seq[SelfEmploymentIncome], ukProperties: Seq[UkPropertyIncome])

object NonSavingsIncomes {
  implicit val employmentIncomeFormats = Json.format[EmploymentIncome]
  implicit val selfEmploymentIncomeFormats = Json.format[SelfEmploymentIncome]
  implicit val ukPropertyIncomeFormats = Json.format[UkPropertyIncome]
  implicit val format = Json.format[NonSavingsIncomes]
}

case class SavingsIncomes(fromUKBanksAndBuildingSocieties: Seq[InterestFromUKBanksAndBuildingSocieties])

object SavingsIncomes {
  implicit val format = Json.format[SavingsIncomes]
}

case class DividendsIncomes(fromUKSources: Seq[DividendsFromUKSources])

object DividendsIncomes {
  implicit val format = Json.format[DividendsIncomes]
}

case class IncomeFromSources(nonSavings: NonSavingsIncomes, savings: SavingsIncomes, dividends: DividendsIncomes, total: BigDecimal)

object IncomeFromSources {
  implicit val format = Json.format[IncomeFromSources]
}

case class Deductions(incomeTaxRelief: BigDecimal, personalAllowance: BigDecimal, retirementAnnuityContract: BigDecimal, total: BigDecimal) {
  require(total >= incomeTaxRelief, "totalDeductions must be greater than or equal to incomeTaxRelief at all times")
}

object Deductions {
  implicit val format = Json.format[Deductions]
}

case class TaxBandSummary(taxBand: String, taxableAmount: BigDecimal, chargedAt: String, tax: BigDecimal)

object TaxBandSummary {
  implicit val format = Json.format[TaxBandSummary]
}

case class IncomeTaxCalculations(nonSavings: Seq[TaxBandSummary], savings: Seq[TaxBandSummary], dividends: Seq[TaxBandSummary], total: BigDecimal)

object IncomeTaxCalculations {
  implicit val format = Json.format[IncomeTaxCalculations]
}

case class IncomeSummary(incomes: IncomeFromSources, deductions: Option[Deductions], totalIncomeOnWhichTaxIsDue: BigDecimal)

object IncomeSummary {
  implicit val format = Json.format[IncomeSummary]
}

case class TaxDeducted(interestFromUk: BigDecimal, total: BigDecimal)

object TaxDeducted {
  implicit val format = Json.format[TaxDeducted]
}

case class Liability(income: IncomeSummary, incomeTaxCalculations: IncomeTaxCalculations, taxDeducted: TaxDeducted, totalTaxDue: BigDecimal, totalTaxOverpaid: BigDecimal)

object Liability {
  implicit val format = Json.format[Liability]

  def example: Liability =
    Liability(
      income = IncomeSummary(
        incomes = IncomeFromSources(
          nonSavings = NonSavingsIncomes(
            employment = exampleEmploymentIncomes,
            selfEmployment = exampleSelfEmploymentIncomes,
            ukProperties = exampleUkPropertiesIncomes
          ),
          savings = SavingsIncomes(
            fromUKBanksAndBuildingSocieties = exampleInterestFromUKBanksAndBuildingSocieties
          ),
          dividends = DividendsIncomes(
            fromUKSources = exampleDividendsFromUKSources
          ),
          total = 93039
        ),
        deductions = Some(Deductions(
          incomeTaxRelief = BigDecimal(5000),
          personalAllowance = BigDecimal(9440),
          retirementAnnuityContract = BigDecimal(10000),
          total = BigDecimal(24440)
        )),
        totalIncomeOnWhichTaxIsDue = BigDecimal(80000)
      ),
      incomeTaxCalculations = IncomeTaxCalculations(
        nonSavings = Seq(
          TaxBandSummary(taxBand = "basicRate", taxableAmount = 10000, chargedAt = "20%", tax = 2000),
          TaxBandSummary("higherRate", 10000, "40%", 4000),
          TaxBandSummary("additionalHigherRate", 10000, "45%", 4500)
        ),
        savings = Seq(
          TaxBandSummary("startingRate", 10000, "0%", 0),
          TaxBandSummary("nilRate", 10000, "0%", 0),
          TaxBandSummary("basicRate", 10000, "20%", 2000),
          TaxBandSummary("higherRate", 10000, "40%", 4000),
          TaxBandSummary("additionalHigherRate", 10000, "45%", 4500)
        ),
        dividends = Seq(
          TaxBandSummary("nilRate", 10000, "0%", 0),
          TaxBandSummary("basicRate", 10000, "20%", 2000),
          TaxBandSummary("higherRate", 10000, "40%", 4000),
          TaxBandSummary("additionalHigherRate", 10000, "45%", 4500)
        ),
        total = 31500
      ),
      taxDeducted = TaxDeducted(
        interestFromUk = 0,
        total = 0
      ),
      totalTaxDue = 25796.95,
      totalTaxOverpaid = 0
    )

  private def exampleEmploymentIncomes = {
    FeatureSwitch(AppContext.featureSwitch).isEnabled(SourceTypes.Employments) match {
      case true =>
        Seq(
          EmploymentIncome("employment-1", 1000, 500, 250, 1250),
          EmploymentIncome("employment-2", 2000, 1000, 500, 2500)
        )
      case false => Seq()
    }
  }

  private def exampleSelfEmploymentIncomes = {
    FeatureSwitch(AppContext.featureSwitch).isEnabled(SourceTypes.SelfEmployments) match {
      case true =>
        Seq(
          SelfEmploymentIncome("self-employment-1", 8200, 10000),
          SelfEmploymentIncome("self-employment-2", 25000, 28000)
        )
      case false => Seq()
    }
  }

  private def exampleUkPropertiesIncomes = {
    FeatureSwitch(AppContext.featureSwitch).isEnabled(SourceTypes.UKProperties) match {
      case true => Seq(
        UkPropertyIncome("property1", profit = 2000, taxableProfit = 1800),
        UkPropertyIncome("property2", profit = 1500, taxableProfit = 1500)
      )

      case false => Seq()
    }
  }

  private def exampleInterestFromUKBanksAndBuildingSocieties = {
    FeatureSwitch(AppContext.featureSwitch).isEnabled(SourceTypes.UnearnedIncomes) match {
      case true =>
        Seq(
          InterestFromUKBanksAndBuildingSocieties("interest-income-1", 100),
          InterestFromUKBanksAndBuildingSocieties("interest-income-2", 200)
        )
      case false => Seq()
    }
  }

  private def exampleDividendsFromUKSources = {
    FeatureSwitch(AppContext.featureSwitch).isEnabled(SourceTypes.UnearnedIncomes) match {
      case true =>
        Seq(
          DividendsFromUKSources("dividend-income-1", 1000),
          DividendsFromUKSources("dividend-income-2", 2000)
        )
      case false => Seq()
    }
  }

}
