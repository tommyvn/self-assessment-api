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

package uk.gov.hmrc.selfassessmentapi.services.live.calculation

import java.util.concurrent.Executors

import uk.gov.hmrc.selfassessmentapi.repositories.domain.{AllowancesAndReliefs, MongoLiability}
import uk.gov.hmrc.selfassessmentapi.services.live.calculation.steps2._

import scala.concurrent.{ExecutionContext, Future}

class LiabilityCalculator2 {

  implicit val executionContext = ExecutionContext.fromExecutorService(Executors.newSingleThreadExecutor())

  def calculate(selfAssessment: SelfAssessment, liability: MongoLiability): Future[MongoLiability] = {
    for {
      selfEmploymentIncomes <- SelfEmploymentProfitCalculation(selfAssessment.selfEmployments)
      interestFromBanks <- UnearnedInterestFromUKBanksAndBuildingSocietiesCalculation(selfAssessment.unearnedIncomes)
      dividendsFromUKSources <- DividendsFromUKSourcesCalculation(selfAssessment.unearnedIncomes)
      (nonSavingsIncomeReceived, totalIncomeReceived, totalTaxableIncome) <- TotalIncomeCalculation(selfEmploymentIncomes, interestFromBanks, dividendsFromUKSources)
      incomeTaxRelief <- IncomeTaxReliefCalculation(selfEmploymentIncomes)
      personalAllowance <- PersonalAllowanceCalculation(totalIncomeReceived , incomeTaxRelief)
      totalAllowancesAndReliefs <- Future {incomeTaxRelief + personalAllowance}
      totalIncomeOnWhichTaxIsDue <- TotalIncomeOnWhichTaxIsDueCalculation(totalIncomeReceived, totalAllowancesAndReliefs)
      personalSavingsAllowance <- PersonalSavingsAllowanceCalculation(totalIncomeOnWhichTaxIsDue)
      savingsStartingRate <- SavingsStartingRateCalculation(nonSavingsIncomeReceived, totalAllowancesAndReliefs)
      (nonSavingsTaxAllocated, deductionsRemainingAfterNonSavings) <- NonSavingsIncomeTaxCalculation(nonSavingsIncomeReceived, totalAllowancesAndReliefs)
      (savingsTaxAllocated, deductionsRemainingAfterSavings) <- SavingsIncomeTaxCalculation(deductionsRemainingAfterNonSavings, personalSavingsAllowance,
                                                                                            savingsStartingRate, interestFromBanks.map(_.totalInterest).sum,
                                                                                            nonSavingsTaxAllocated)
      (dividendsTaxAllocated, deductionsRemainingAfterDividends) <- DividendsTaxCalculation(deductionsRemainingAfterSavings, nonSavingsTaxAllocated,
                                                                                            savingsTaxAllocated, dividendsFromUKSources)
      taxDeducted <- TaxDeductedCalculation(selfAssessment.unearnedIncomes)
    } yield liability.copy(profitFromSelfEmployments = selfEmploymentIncomes,
                           interestFromUKBanksAndBuildingSocieties = interestFromBanks,
                           dividendsFromUKSources = dividendsFromUKSources,
                           nonSavingsIncomeReceived = Some(nonSavingsIncomeReceived),
                           totalIncomeReceived = Some(totalIncomeReceived),
                           totalTaxableIncome = Some(totalTaxableIncome),
                           allowancesAndReliefs = AllowancesAndReliefs(incomeTaxRelief = Some(incomeTaxRelief),
                                                                       personalAllowance = Some(personalAllowance),
                                                                       personalSavingsAllowance = Some(personalSavingsAllowance),
                                                                       savingsStartingRate = Some(savingsStartingRate)),
                           totalAllowancesAndReliefs = Some(totalAllowancesAndReliefs),
                           totalIncomeOnWhichTaxIsDue = Some(totalIncomeOnWhichTaxIsDue),
                           nonSavingsIncome = nonSavingsTaxAllocated,
                           savingsIncome = savingsTaxAllocated,
                           dividendsIncome = dividendsTaxAllocated,
                           taxDeducted = Option(taxDeducted))
  }
}

object LiabilityCalculator2 {
  def apply() = new LiabilityCalculator2()
}