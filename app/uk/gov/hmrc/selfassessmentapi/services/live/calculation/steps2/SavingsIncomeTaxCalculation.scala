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

package uk.gov.hmrc.selfassessmentapi.services.live.calculation.steps2

import uk.gov.hmrc.selfassessmentapi.repositories.domain.TaxBand._
import uk.gov.hmrc.selfassessmentapi.repositories.domain.{TaxBand, TaxBandAllocation}

import scala.concurrent.Future

object SavingsIncomeTaxCalculation extends CalculationStep {

  def apply(personalSavingsAllowance: BigDecimal, savingsStartingRate: BigDecimal, deductionsAfterNonSavings: BigDecimal,
            totalSavingsIncome: BigDecimal, nonSavingsTaxAllocated: Seq[TaxBandAllocation]) = Future[(Seq[TaxBandAllocation], BigDecimal)] {

    val taxBands = Seq(
      TaxBandState(taxBand = SavingsStartingTaxBand, available = savingsStartingRate),
      TaxBandState(taxBand = NilTaxBand, available = personalSavingsAllowance),
      TaxBandState(taxBand = BasicTaxBand, available = positiveOrZero(availableAfterNonSavings(BasicTaxBand, nonSavingsTaxAllocated) - savingsStartingRate - personalSavingsAllowance)),
      TaxBandState(taxBand = HigherTaxBand, available = availableAfterNonSavings(HigherTaxBand, nonSavingsTaxAllocated)),
      TaxBandState(taxBand = AdditionalHigherTaxBand, available = availableAfterNonSavings(AdditionalHigherTaxBand, nonSavingsTaxAllocated))
    )

    val (taxableSavingsIncome, deductionsRemaining) = applyDeductions(totalSavingsIncome, deductionsAfterNonSavings)

    (allocateToTaxBands(taxableSavingsIncome, taxBands), deductionsRemaining)
  }
  
  private def availableAfterNonSavings(taxBand: TaxBand, nonSavingsTaxAllocated: Seq[TaxBandAllocation]) =
    nonSavingsTaxAllocated.find(_.taxBand == taxBand).map(_.available).getOrElse(taxBand.width)
}
