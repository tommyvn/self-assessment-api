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

import uk.gov.hmrc.selfassessmentapi.domain.{DividendsFromUKSources, InterestFromUKBanksAndBuildingSocieties}
import uk.gov.hmrc.selfassessmentapi.repositories.domain.SelfEmploymentIncome

import scala.concurrent.{ExecutionContext, Future}

object TotalIncomeCalculation {
  def apply(profitFromSelfEmployments : Seq[SelfEmploymentIncome],
            interestFromUKBanksAndBuildingSocieties: Seq[InterestFromUKBanksAndBuildingSocieties],
            dividendsFromUKSources: Seq[DividendsFromUKSources])(implicit ec: ExecutionContext) = Future[(BigDecimal, BigDecimal,BigDecimal)] {
    val (profitsFromSelfEmployment, taxableProfits) = profitFromSelfEmployments.map(income => (income.profit, income.taxableProfit)).unzip
    val nonSavingsIncomeReceived = profitsFromSelfEmployment.sum
    val savingsIncomeReceived = interestFromUKBanksAndBuildingSocieties.map(_.totalInterest).sum
    val dividendsIncomeReceived = dividendsFromUKSources.map(_.totalDividend).sum
    val totalIncomeReceived = nonSavingsIncomeReceived + savingsIncomeReceived + dividendsIncomeReceived
    (nonSavingsIncomeReceived, totalIncomeReceived, taxableProfits.sum)
  }
}
