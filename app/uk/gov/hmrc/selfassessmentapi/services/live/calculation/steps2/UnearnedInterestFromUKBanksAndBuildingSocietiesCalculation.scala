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

import uk.gov.hmrc.selfassessmentapi.domain.InterestFromUKBanksAndBuildingSocieties
import uk.gov.hmrc.selfassessmentapi.domain.unearnedincome.SavingsIncomeType._
import uk.gov.hmrc.selfassessmentapi.repositories.domain.MongoUnearnedIncome

import scala.concurrent.{ExecutionContext, Future}

object UnearnedInterestFromUKBanksAndBuildingSocietiesCalculation extends Math {

  def apply(unearnedIncomes: Seq[MongoUnearnedIncome])(implicit ec: ExecutionContext) = Future[Seq[InterestFromUKBanksAndBuildingSocieties]]{
    unearnedIncomes.map { unearnedIncome =>
      val totalInterest = unearnedIncome.savings.map { saving =>
        saving.`type` match {
          case InterestFromBanksTaxed => saving.amount * 100 / 80
          case InterestFromBanksUntaxed => saving.amount
        }
      }.sum
      InterestFromUKBanksAndBuildingSocieties(unearnedIncome.sourceId, roundDown(totalInterest))
    }
  }
}
