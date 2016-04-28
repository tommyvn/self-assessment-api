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

package uk.gov.hmrc.selfassessmentapi.controllers

import play.api.libs.json.{JsObject, Json}
import play.api.mvc.{Action, AnyContent}
import play.api.mvc.hal._
import uk.gov.hmrc.api.controllers.HeaderValidator
import uk.gov.hmrc.domain.SaUtr
import uk.gov.hmrc.selfassessmentapi.domain.Liability

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

trait LiabilityController extends BaseController with HeaderValidator with Links {

  def requestLiability(utr: SaUtr, taxPeriod: Option[String]): Action[AnyContent]
  def retrieveLiability(utr: SaUtr, liabilityId: String): Action[AnyContent]

}
