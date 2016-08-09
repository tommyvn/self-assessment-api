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

import uk.gov.hmrc.selfassessmentapi.domain.DividendsFromUKSources
import uk.gov.hmrc.selfassessmentapi.repositories.domain.TaxBand._
import uk.gov.hmrc.selfassessmentapi.repositories.domain.{TaxBand, TaxBandAllocation}

import scala.concurrent.{ExecutionContext, Future}


object DividendsTaxCalculation extends CalculationStep {

  private val dividendAllowance = 5000

  def apply(deductions: BigDecimal, nonSavingsTaxAllocated: Seq[TaxBandAllocation], savingsTaxAllocated: Seq[TaxBandAllocation],
            dividendsFromUKSources: Seq[DividendsFromUKSources])(implicit ec: ExecutionContext) = Future[(Seq[TaxBandAllocation], BigDecimal)]{

    val availabilities = List((available(BasicTaxBand, nonSavingsTaxAllocated, savingsTaxAllocated), BigDecimal(0)),
                              (available(HigherTaxBand, nonSavingsTaxAllocated, savingsTaxAllocated), BigDecimal(0)))
    val taxExemptIncome = List((BigDecimal(0), getTaxExemptIncome(savingsTaxAllocated, dividendsFromUKSources)))
    val adjustedAvailabilities = availabilities.foldLeft(taxExemptIncome)(calcAvailability).reverse.tail.unzip._1

    val taxBands = Seq(
      TaxBandState(taxBand = NilTaxBand, available = dividendAllowance),
      TaxBandState(taxBand = BasicTaxBand, available = adjustedAvailabilities(0)),
      TaxBandState(taxBand = HigherTaxBand, available = adjustedAvailabilities(1)),
      TaxBandState(taxBand = AdditionalHigherTaxBand, available = AdditionalHigherTaxBand.width)
    )

    val (taxableDividendsIncome, deductionsRemaining) = applyDeductions(dividendsFromUKSources.map(_.totalDividend).sum, deductions)

    (allocateToTaxBands(taxableDividendsIncome, taxBands), deductionsRemaining)
  }

  private def getTaxExemptIncome(savingsTaxAllocated: Seq[TaxBandAllocation], dividendsFromUKSources: Seq[DividendsFromUKSources]): BigDecimal = {
    sum(savingsTaxAllocated.find(_.taxBand == NilTaxBand).map(_.amount),
      savingsTaxAllocated.find(_.taxBand == SavingsStartingTaxBand).map(_.amount),
      Some(capAt(dividendsFromUKSources.map(_.totalDividend).sum, dividendAllowance)))
  }

  private def available(taxBand: TaxBand, nonSavingsTaxAllocated: Seq[TaxBandAllocation], savingsTaxAllocated: Seq[TaxBandAllocation]) = {
    (for {
      fromSavings <- savingsTaxAllocated.find(_.taxBand == taxBand)
      fromPpp <- nonSavingsTaxAllocated.find(_.taxBand == taxBand)
    } yield (fromPpp + fromSavings).available).getOrElse(taxBand.width)
  }

  private def calcAvailability(input : List[(BigDecimal, BigDecimal)], elem: (BigDecimal, BigDecimal)) : List[(BigDecimal, BigDecimal)] = {
    val availability = elem._1 - input.head._2
    (if (availability >= 0) (availability, BigDecimal(0)) else (BigDecimal(0), availability.abs)) :: input
  }
}
