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

import org.joda.time.{DateTime, DateTimeZone, LocalDate}
import play.api.libs.json._
import reactivemongo.bson.{BSONDocument, BSONDouble, BSONObjectID, BSONString}
import uk.gov.hmrc.domain._
import uk.gov.hmrc.mongo.json.ReactiveMongoFormats
import uk.gov.hmrc.selfassessmentapi.domain.pensioncontribution.PensionContribution
import uk.gov.hmrc.selfassessmentapi.domain.selfemployment.BalancingChargeType.BalancingChargeType
import uk.gov.hmrc.selfassessmentapi.domain.selfemployment.ExpenseType.ExpenseType
import uk.gov.hmrc.selfassessmentapi.domain.selfemployment.IncomeType.IncomeType
import uk.gov.hmrc.selfassessmentapi.domain.selfemployment._
import uk.gov.hmrc.selfassessmentapi.domain.{Income => _, _}

import scala.collection.immutable.Nil

case class MongoSelfEmploymentIncomeSummary(summaryId: SummaryId,
                                            `type`: IncomeType,
                                            amount: BigDecimal) extends MongoSummary[Income] {
  val arrayName = MongoSelfEmploymentIncomeSummary.arrayName

  def toDomain: Income =
    Income(id = Some(summaryId),
      `type` = `type`,
      amount = amount)

  def toBsonDocument = BSONDocument(
    "summaryId" -> summaryId,
    "amount" -> BSONDouble(amount.doubleValue()),
    "type" -> BSONString(`type`.toString)
  )
}

object MongoSelfEmploymentIncomeSummary {

  val arrayName = "incomes"

  implicit val format = Json.format[MongoSelfEmploymentIncomeSummary]

  def toMongoSummary(income: Income, id: Option[SummaryId] = None) = {
    new MongoSelfEmploymentIncomeSummary(
      summaryId = id.getOrElse(BSONObjectID.generate.stringify),
      `type` = income.`type`,
      amount = income.amount
    )
  }
}

case class MongoSelfEmploymentExpenseSummary(summaryId: SummaryId,
                                             `type`: ExpenseType,
                                             amount: BigDecimal) extends MongoSummary[Expense] {
  val arrayName = MongoSelfEmploymentExpenseSummary.arrayName

  def toDomain: Expense =
    Expense(id = Some(summaryId),
      `type` = `type`,
      amount = amount)

  def toBsonDocument = BSONDocument(
    "summaryId" -> summaryId,
    "amount" -> BSONDouble(amount.doubleValue()),
    "type" -> BSONString(`type`.toString)
  )
}

object MongoSelfEmploymentExpenseSummary {

  val arrayName = "expenses"

  implicit val format = Json.format[MongoSelfEmploymentExpenseSummary]

  def toMongoSummary(expense: Expense, id: Option[SummaryId] = None): MongoSelfEmploymentExpenseSummary = {
    new MongoSelfEmploymentExpenseSummary(
      summaryId = id.getOrElse(BSONObjectID.generate.stringify),
      `type` = expense.`type`,
      amount = expense.amount
    )
  }
}

case class MongoSelfEmploymentBalancingChargeSummary(summaryId: SummaryId,
                                                     `type`: BalancingChargeType,
                                                     amount: BigDecimal) extends MongoSummary[BalancingCharge] {
  val arrayName = MongoSelfEmploymentBalancingChargeSummary.arrayName

  def toDomain =
    BalancingCharge(id = Some(summaryId),
      `type` = `type`,
      amount = amount)

  def toBsonDocument = BSONDocument(
    "summaryId" -> summaryId,
    "amount" -> BSONDouble(amount.doubleValue()),
    "type" -> BSONString(`type`.toString)
  )
}

object MongoSelfEmploymentBalancingChargeSummary {

  val arrayName = "balancingCharges"

  implicit val format = Json.format[MongoSelfEmploymentBalancingChargeSummary]

  def toMongoSummary(balancingCharge: BalancingCharge, id: Option[SummaryId] = None): MongoSelfEmploymentBalancingChargeSummary = {
    new MongoSelfEmploymentBalancingChargeSummary(
      summaryId = id.getOrElse(BSONObjectID.generate.stringify),
      `type` = balancingCharge.`type`,
      amount = balancingCharge.amount
    )
  }
}

case class MongoSelfEmploymentGoodsAndServicesOwnUseSummary(summaryId: SummaryId, amount: BigDecimal) extends MongoSummary[GoodsAndServicesOwnUse] {

  val arrayName = MongoSelfEmploymentGoodsAndServicesOwnUseSummary.arrayName

  def toDomain = GoodsAndServicesOwnUse(id = Some(summaryId), amount = amount)

  def toBsonDocument = BSONDocument(
    "summaryId" -> summaryId,
    "amount" -> BSONDouble(amount.doubleValue())
  )
}

object MongoSelfEmploymentGoodsAndServicesOwnUseSummary {

  val arrayName = "goodsAndServicesOwnUse"

  implicit val format = Json.format[MongoSelfEmploymentGoodsAndServicesOwnUseSummary]

  def toMongoSummary(goodsAndServicesOwnUse: GoodsAndServicesOwnUse, id: Option[SummaryId] = None):
  MongoSelfEmploymentGoodsAndServicesOwnUseSummary = {
    new MongoSelfEmploymentGoodsAndServicesOwnUseSummary(
      summaryId = id.getOrElse(BSONObjectID.generate.stringify),
      amount = goodsAndServicesOwnUse.amount
    )
  }
}

case class MongoTaxYearProperties(id: BSONObjectID,
                                  saUtr: SaUtr,
                                  taxYear: TaxYear,
                                  lastModifiedDateTime: DateTime,
                                  createdDateTime: DateTime,
                                  pensionContributions: Option[PensionContribution] = None)
  extends SelfAssessmentMetadata {

  def toTaxYearProperties = TaxYearProperties(
    id = Some(id.stringify),
    pensionContributions = pensionContributions)
}

object MongoTaxYearProperties {

  implicit val dateTimeFormat = ReactiveMongoFormats.dateTimeFormats

  implicit val mongoFormats = ReactiveMongoFormats.mongoEntity({
    implicit val BSONObjectIDFormat: Format[BSONObjectID] = ReactiveMongoFormats.objectIdFormats
    implicit val dateTimeFormat: Format[DateTime] = ReactiveMongoFormats.dateTimeFormats
    Format(Json.reads[MongoTaxYearProperties], Json.writes[MongoTaxYearProperties])
  })
}

trait MongoSummaries[T] {
  def summaries: Seq[MongoSummary[T]]
  def toDomains = summaries.map(_.toDomain)
  def exists(id: SummaryId): Boolean = summaries.exists(_.summaryId == id)
  def findById(id: SummaryId) = summaries.find(_.summaryId == id).map(_.toDomain)
  def total = Some(summaries.map(_.amount).sum)
}

case class SelfEmploymentIncomes(incomes: Seq[MongoSelfEmploymentIncomeSummary]) extends MongoSummaries[Income] {
  override def summaries = incomes
}

object SelfEmploymentIncomes {
  implicit val format = Json.format[SelfEmploymentIncomes]
}

case class SelfEmploymentBalancingCharges(balancingCharges: Seq[MongoSelfEmploymentBalancingChargeSummary]) extends MongoSummaries[BalancingCharge] {
  override def summaries: Seq[MongoSummary[BalancingCharge]] = balancingCharges
}

object SelfEmploymentBalancingCharges {
  implicit val format = Json.format[SelfEmploymentBalancingCharges]
}

case class SelfEmploymentExpenses(expenses: Seq[MongoSelfEmploymentExpenseSummary]) extends MongoSummaries[Expense] {
  def totalAmountExceptType(expenseType: ExpenseType.Value) = {
    expenses.filterNot(_.`type` == expenseType).map(_.amount).sum
  }

  override def summaries: Seq[MongoSummary[Expense]] = expenses
}

object SelfEmploymentExpenses {
  implicit val format = Json.format[SelfEmploymentExpenses]
}

case class SelfEmploymentGoodAndServicesOwnUse(goodsAndServicesOwnUse: Seq[MongoSelfEmploymentGoodsAndServicesOwnUseSummary]) extends MongoSummaries[GoodsAndServicesOwnUse] {
  override def summaries: Seq[MongoSummary[GoodsAndServicesOwnUse]] = goodsAndServicesOwnUse
}

object SelfEmploymentGoodAndServicesOwnUse {
  implicit val format = Json.format[SelfEmploymentGoodAndServicesOwnUse]
}


case class MongoSelfEmployment(id: BSONObjectID,
                               sourceId: SourceId,
                               saUtr: SaUtr,
                               taxYear: TaxYear,
                               lastModifiedDateTime: DateTime,
                               createdDateTime: DateTime,
                               commencementDate: LocalDate,
                               allowances: Option[Allowances] = None,
                               adjustments: Option[Adjustments] = None,
                               incomes: SelfEmploymentIncomes = SelfEmploymentIncomes(Nil),
                               expenses: SelfEmploymentExpenses = SelfEmploymentExpenses(Nil),
                               balancingCharges: SelfEmploymentBalancingCharges = SelfEmploymentBalancingCharges(Nil),
                               goodsAndServicesOwnUse: SelfEmploymentGoodAndServicesOwnUse = SelfEmploymentGoodAndServicesOwnUse(Nil)) extends SourceMetadata {

  def toSelfEmployment = SelfEmployment(
    id = Some(sourceId),
    commencementDate = commencementDate,
    allowances = allowances,
    adjustments = adjustments)

  def profitIncreases: BigDecimal = {
    Sum(incomes.total, balancingCharges.total, goodsAndServicesOwnUse.total, adjustments.map(_.total))
  }
}


object MongoSelfEmployment {
  implicit val dateTimeFormat = ReactiveMongoFormats.dateTimeFormats
  implicit val localDateFormat = ReactiveMongoFormats.localDateFormats

  implicit val mongoFormats = ReactiveMongoFormats.mongoEntity({
    implicit val BSONObjectIDFormat: Format[BSONObjectID] = ReactiveMongoFormats.objectIdFormats
    implicit val dateTimeFormat: Format[DateTime] = ReactiveMongoFormats.dateTimeFormats
    implicit val localDateFormat: Format[LocalDate] = ReactiveMongoFormats.localDateFormats
    Format(Json.reads[MongoSelfEmployment], Json.writes[MongoSelfEmployment])
  })

  def create(saUtr: SaUtr, taxYear: TaxYear, se: SelfEmployment): MongoSelfEmployment = {
    val id = BSONObjectID.generate
    val now = DateTime.now(DateTimeZone.UTC)
    MongoSelfEmployment(
      id = id,
      sourceId = id.stringify,
      saUtr = saUtr,
      taxYear = taxYear,
      lastModifiedDateTime = now,
      createdDateTime = now,
      commencementDate = se.commencementDate,
      allowances = se.allowances,
      adjustments = se.adjustments)
  }
}





