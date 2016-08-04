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

import org.scalacheck.Gen
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.Tables.Table
import uk.gov.hmrc.selfassessmentapi.domain.selfemployment._
import uk.gov.hmrc.selfassessmentapi.domain.unearnedincome.SavingsIncomeType._
import uk.gov.hmrc.selfassessmentapi.repositories.domain.TaxBand.{AdditionalHigherTaxBand, BasicTaxBand, HigherTaxBand}
import uk.gov.hmrc.selfassessmentapi.services.live.calculation.steps.SelfAssessment
import uk.gov.hmrc.selfassessmentapi.{SelfEmploymentSugar, UnitSpec, domain}

class FunctionalLiabilitySpec extends UnitSpec with SelfEmploymentSugar {

  private val selfEmploymentId = "selfEmploymentId"

  "Taxable Profit from self employment" should {
    "be equal to the sum of all incomes, balancingCharges, goodsAndServices and basisAdjustment, accountingAdjustment and averagingAdjustment" in {

      val selfEmployment =
        aSelfEmployment(selfEmploymentId).copy(
          incomes = Seq(
            income(IncomeType.Turnover, 1200.01),
            income(IncomeType.Other, 799.99)
          ),
          balancingCharges = Seq(
            balancingCharge(BalancingChargeType.BPRA, 10),
            balancingCharge(BalancingChargeType.Other, 20)
          ),
          goodsAndServicesOwnUse = Seq(
            goodsAndServices(50)
          ),
          adjustments = Some(Adjustments(
            basisAdjustment = Some(200),
            accountingAdjustment = Some(100),
            averagingAdjustment = Some(50)
          ))
        )

      ProfitFromSelfEmploymentWithoutLossBroughtForward(selfEmployment) shouldBe BigDecimal(2430)

    }

    "be equal to incomes and outstandingBusinessIncome" in {

      val selfEmployment =
        aSelfEmployment(selfEmploymentId).copy(
          incomes = Seq(
            income(IncomeType.Turnover, 2000)
          ),
          adjustments = Some(Adjustments(
            outstandingBusinessIncome = Some(3000)
          ))
        )

      ProfitFromSelfEmploymentWithoutLossBroughtForward(selfEmployment) shouldBe BigDecimal(5000)

    }

    "not contain any expenses apart from depreciation" in {

      val selfEmployment =
        aSelfEmployment(selfEmploymentId).copy(
          incomes = Seq(
            income(IncomeType.Turnover, 2000)
          ),
          expenses = Seq(
            expense(ExpenseType.AdminCosts, 100),
            expense(ExpenseType.BadDebt, 50.01),
            expense(ExpenseType.CISPayments, 49.99),
            expense(ExpenseType.Depreciation, 1000000)
          )
        )

      ProfitFromSelfEmploymentWithoutLossBroughtForward(selfEmployment) shouldBe BigDecimal(1800)
    }

    "subtract all allowances from profit" in {

      val selfEmployment =
        aSelfEmployment(selfEmploymentId).copy(
          incomes = Seq(
            income(IncomeType.Turnover, 2000)
          ),
          allowances = Some(Allowances(
            annualInvestmentAllowance = Some(50),
            capitalAllowanceMainPool = Some(10),
            capitalAllowanceSpecialRatePool = Some(10),
            restrictedCapitalAllowance = Some(10),
            businessPremisesRenovationAllowance = Some(10),
            enhancedCapitalAllowance = Some(4.99),
            allowancesOnSales = Some(5.01)
          ))
        )

      ProfitFromSelfEmploymentWithoutLossBroughtForward(selfEmployment) shouldBe BigDecimal(1900)

    }

    "be rounded down to the nearest pound" in {

      val selfEmployment =
        aSelfEmployment(selfEmploymentId).copy(
          incomes = Seq(
            income(IncomeType.Turnover, 1.99)
          ),
          allowances = Some(Allowances(
            annualInvestmentAllowance = Some(0.02)
          ))
        )

      ProfitFromSelfEmploymentWithoutLossBroughtForward(selfEmployment) shouldBe BigDecimal(1)
    }

    "subtract adjustments from profit" in {

      val selfEmployment =
        aSelfEmployment(selfEmploymentId).copy(
          incomes = Seq(
            income(IncomeType.Turnover, 2000)
          ),
          adjustments = Some(Adjustments(
            includedNonTaxableProfits = Some(50),
            basisAdjustment = Some(-15),
            overlapReliefUsed = Some(10),
            averagingAdjustment = Some(-25)
          ))
        )

      ProfitFromSelfEmploymentWithoutLossBroughtForward(selfEmployment) shouldBe BigDecimal(1900)

    }

    "reduce cap annualInvestmentAllowance at 200000" in {

      val selfEmployment =
        aSelfEmployment(selfEmploymentId).copy(
          incomes = Seq(
            income(IncomeType.Turnover, 230000)
          ),
          allowances = Some(Allowances(
            annualInvestmentAllowance = Some(230000)
          ))
        )

      ProfitFromSelfEmploymentWithoutLossBroughtForward(selfEmployment) shouldBe BigDecimal(30000)

    }

    "be reduced by lossBroughtForward" in {

      val selfEmployment =
        aSelfEmployment(selfEmploymentId).copy(
          incomes = Seq(
            income(IncomeType.Turnover, 2000)
          ),
          adjustments = Some(Adjustments(
            lossBroughtForward = Some(1001)
          ))
        )

      ProfitFromSelfEmploymentWithoutLossBroughtForward(selfEmployment) shouldBe BigDecimal(999)
    }

    "return zero as taxable profit if lossBroughtForward is greater than adjusted profit" in {

      val selfEmployment =
        aSelfEmployment(selfEmploymentId).copy(
          incomes = Seq(
            income(IncomeType.Turnover, 2000)
          ),
          adjustments = Some(Adjustments(
            lossBroughtForward = Some(3000)
          ))
        )

      ProfitFromSelfEmploymentWithoutLossBroughtForward(selfEmployment) shouldBe BigDecimal(0)

    }

    "be zero if expenses are bigger than incomes (loss)" in {

      val selfEmployment =
        aSelfEmployment(selfEmploymentId).copy(
          incomes = Seq(
            income(IncomeType.Turnover, 2000)
          ),
          expenses = Seq(
            expense(ExpenseType.AdminCosts, 4000)
          ),
          adjustments = Some(Adjustments(
            lossBroughtForward = Some(1000)
          ))
        )

      ProfitFromSelfEmploymentWithoutLossBroughtForward(selfEmployment) shouldBe BigDecimal(0)
    }

  }

  "Profit from Self employments" should {
    "be equal to the sum of taxableProfit and lossBroughtForward" in {
      ProfitFromSelfEmployment(adjustedProfits = 100, outstandingBusinessIncome = 200) shouldBe 300
    }
  }

  "Personal Allowance" should {
    "be 11,000 if total taxable income is less than 100,000" in {
      PersonalAllowance(totalIncomeReceived = 99999) shouldBe 11000
    }

    "be 11,000 if total taxable income is 100,001" in {
      PersonalAllowance(totalIncomeReceived = 100001) shouldBe 11000
    }

    "be 11,000 - (TotalIncome - 100,000)/2 if total taxable income is greater than 100,000 but less than 122,000" in {
      PersonalAllowance(totalIncomeReceived = 121999) shouldBe (11000 - (121999 - 100000)/2)
      PersonalAllowance(totalIncomeReceived = 120000) shouldBe (11000 - (120000 - 100000)/2)
      PersonalAllowance(totalIncomeReceived = 110000) shouldBe (11000 - (110000 - 100000)/2)
    }

    "be 0 if total taxable income is greater than equal to 122,000" in {
      PersonalAllowance(totalIncomeReceived = 122000) shouldBe 0
      PersonalAllowance(totalIncomeReceived = 122001) shouldBe 0
      PersonalAllowance(totalIncomeReceived = 132000) shouldBe 0
    }
  }

  "Loss brought forward" should {
    "be equal self employment loss brought forward" in {
      val selfEmployment =
        aSelfEmployment(selfEmploymentId).copy(
          adjustments = Some(Adjustments(
            lossBroughtForward = Some(999)
          ))
        )

      LossBroughtForward(selfEmployment, 1000) shouldBe 999
    }

    "be capped at adjusted profits" in {
      val selfEmployment =
        aSelfEmployment(selfEmploymentId).copy(
          adjustments = Some(Adjustments(
            lossBroughtForward = Some(1001)
          ))
        )

      LossBroughtForward(selfEmployment, 1000) shouldBe 1000
    }

    "be 0 if none is provided" in {
      val selfEmployment =
        aSelfEmployment(selfEmploymentId).copy(
          adjustments = None
        )

      LossBroughtForward(selfEmployment, 1000) shouldBe 0
    }
  }

  "Interest from UK banks and building societies" should {
    def taxedInterest(amount: BigDecimal) = MongoUnearnedIncomesSavingsIncomeSummary("", InterestFromBanksTaxed, amount)
    def unTaxedInterest(amount: BigDecimal) = MongoUnearnedIncomesSavingsIncomeSummary("", InterestFromBanksUntaxed, amount)

    "calculate rounded down interest when there are multiple interest of both taxed and unTaxed from uk banks and building societies from multiple unearned income source" in {

        val unearnedIncomes1 = anUnearnedIncomes().copy(savings = Seq(taxedInterest(100.50), unTaxedInterest(200.50)))
        val unearnedIncomes2 = anUnearnedIncomes().copy(savings = Seq(taxedInterest(300.99), unTaxedInterest(400.99)))

      InterestFromUKBanksAndBuildingSocieties(SelfAssessment(unearnedIncomes = Seq(unearnedIncomes1, unearnedIncomes2))) should contain theSameElementsAs
        Seq(domain.InterestFromUKBanksAndBuildingSocieties(sourceId = unearnedIncomes1.sourceId, BigDecimal(326)),
        domain.InterestFromUKBanksAndBuildingSocieties(sourceId = unearnedIncomes2.sourceId, BigDecimal(777)))
      }

    "calculate interest when there is one taxed interest from uk banks and building societies from a single unearned income source" in {
      val unearnedIncomes = anUnearnedIncomes().copy(savings = Seq(taxedInterest(100)))

      InterestFromUKBanksAndBuildingSocieties(SelfAssessment(unearnedIncomes = Seq(unearnedIncomes))) should contain theSameElementsAs
        Seq(domain.InterestFromUKBanksAndBuildingSocieties(unearnedIncomes.sourceId, BigDecimal(125)))

    }

    "calculate interest when there are multiple taxed interest from uk banks and building societies from a single unearned income source" in {
      val unearnedIncomes = anUnearnedIncomes().copy(savings = Seq(taxedInterest(100), taxedInterest(200)))

      InterestFromUKBanksAndBuildingSocieties(SelfAssessment(unearnedIncomes = Seq(unearnedIncomes))) should contain theSameElementsAs
        Seq(domain.InterestFromUKBanksAndBuildingSocieties(unearnedIncomes.sourceId, BigDecimal(375)))

    }

    "calculate round down interest when there is one taxed interest from uk banks and building societies from a single unearned income source" in {
      val unearnedIncomes = anUnearnedIncomes().copy(savings = Seq(taxedInterest(100.50)))

      InterestFromUKBanksAndBuildingSocieties(SelfAssessment(unearnedIncomes = Seq(unearnedIncomes))) should contain theSameElementsAs
        Seq(domain.InterestFromUKBanksAndBuildingSocieties(unearnedIncomes.sourceId, BigDecimal(125)))
    }

    "calculate round down interest when there are multiple taxed interest from uk banks and building societies from a single unearned income source" in {
      val unearnedIncomes = anUnearnedIncomes().copy(savings = Seq(taxedInterest(100.90), taxedInterest(200.99)))

      InterestFromUKBanksAndBuildingSocieties(SelfAssessment(unearnedIncomes = Seq(unearnedIncomes))) should contain theSameElementsAs
        Seq(domain.InterestFromUKBanksAndBuildingSocieties(unearnedIncomes.sourceId, BigDecimal(377)))

    }

    "calculate interest when there is one unTaxed interest from uk banks and building societies from a single unearned income source" in {
      val unearnedIncomes = anUnearnedIncomes().copy(savings = Seq(unTaxedInterest(100)))

      InterestFromUKBanksAndBuildingSocieties(SelfAssessment(unearnedIncomes = Seq(unearnedIncomes))) should contain theSameElementsAs
        Seq(domain.InterestFromUKBanksAndBuildingSocieties(unearnedIncomes.sourceId, BigDecimal(100)))

    }

    "calculate interest when there are multiple unTaxed interest from uk banks and building societies from a single unearned income source" in {
      val unearnedIncomes = anUnearnedIncomes().copy(savings = Seq(unTaxedInterest(100), unTaxedInterest(200)))

      InterestFromUKBanksAndBuildingSocieties(SelfAssessment(unearnedIncomes = Seq(unearnedIncomes))) should contain theSameElementsAs
        Seq(domain.InterestFromUKBanksAndBuildingSocieties(unearnedIncomes.sourceId, BigDecimal(300)))
    }


    "calculate rounded down interest when there is one unTaxed interest from uk banks and building societies from a single unearned income source" in {
      val unearnedIncomes = anUnearnedIncomes().copy(savings = Seq(unTaxedInterest(100.50)))

      InterestFromUKBanksAndBuildingSocieties(SelfAssessment(unearnedIncomes = Seq(unearnedIncomes))) should contain theSameElementsAs
        Seq(domain.InterestFromUKBanksAndBuildingSocieties(unearnedIncomes.sourceId, BigDecimal(100)))
    }

    "calculate rounded down interest when there are multiple unTaxed interest from uk banks and building societies from a single unearned income source" in {
      val unearnedIncomes = anUnearnedIncomes().copy(savings = Seq(unTaxedInterest(100.50), unTaxedInterest(200.99)))

      InterestFromUKBanksAndBuildingSocieties(SelfAssessment(unearnedIncomes = Seq(unearnedIncomes))) should contain theSameElementsAs
        Seq(domain.InterestFromUKBanksAndBuildingSocieties(unearnedIncomes.sourceId, BigDecimal(301)))

    }

  }

  "total income" should {
    "calculate total income" in {
      TotalIncomeReceived(totalProfitFromSelfEmployments = 200, totalSavings = 250, totalDividends = 3000) shouldBe 3450
    }

    "calculate total income if there is no income from self employments" in {
      TotalIncomeReceived(totalProfitFromSelfEmployments = 0, totalSavings = 0, totalDividends = 0) shouldBe 0
    }

    "calculate total income if there is no income from self employments but has interest from UK banks and building societies" in {
      TotalIncomeReceived(totalProfitFromSelfEmployments = 0, totalSavings = 250, totalDividends = 0) shouldBe 250
    }

    "calculate total income if there is no income from self employments but has dividends from unearned income" in {
      TotalIncomeReceived(totalProfitFromSelfEmployments = 0, totalSavings = 0, totalDividends = 3000) shouldBe 3000
    }
  }

  "Total deduction" should {
    "be sum of income tax relief and personal allowance" in {
      TotalDeduction(incomeTaxRelief = 1000.00, personalAllowance = 10000.00) shouldBe 11000
    }
  }

  "Income tax Relief" should {
    "rounded up sum of all self employments loss brought forward values" in {

      val selfEmploymentOne =
        aSelfEmployment(selfEmploymentId).copy(
          incomes = Seq(income(IncomeType.Turnover, 2000)),
          adjustments = Some(Adjustments(lossBroughtForward = Some(200.25)))
        )

      val selfEmploymentTwo =
        aSelfEmployment(selfEmploymentId).copy(
          incomes = Seq(income(IncomeType.Turnover, 2000)),
          adjustments = Some(Adjustments(lossBroughtForward = Some(100.34)))
        )

      IncomeTaxRelief(SelfAssessment(selfEmployments = Seq(selfEmploymentOne, selfEmploymentTwo))) shouldBe 301

    }
  }

  "Total incomes on which tax is due" should {
    "be totalIncomeReceived - totalDeduction" in {
      TotalTaxableIncome(totalIncomeReceived = 100, totalDeduction = 50) shouldBe 50
    }

    "zero if totalIncomeReceived is less than totalDeductions" in {
      TotalTaxableIncome(totalIncomeReceived = 50, totalDeduction = 100) shouldBe 0
    }

  }

  "PersonalSavingsAllowance" should {
     def generate(lowerLimit: Int, upperLimit: Int) = for { value <- Gen.chooseNum(lowerLimit, upperLimit) } yield value

    "be zero when the total income on which tax is due is zero" in {
      PersonalSavingsAllowance(0) shouldBe 0
    }

    "be 1000 when the total income on which tax is due is less than equal to 32000 " in {
      PersonalSavingsAllowance(1) shouldBe 1000
      generate(1, 32000) map { randomNumber => PersonalSavingsAllowance(randomNumber) shouldBe 1000 }
      PersonalSavingsAllowance(32000) shouldBe 1000
    }

    "be 500 when the total income on which tax is due is greater than 32000 but less than equal to 150000" in {
      PersonalSavingsAllowance(32001) shouldBe 500
      generate(32001, 150000) map { randomNumber => PersonalSavingsAllowance(randomNumber) shouldBe 500 }
      PersonalSavingsAllowance(150000) shouldBe 500
    }

    "be 0 when the total income on which tax is due is greater than 150000" in {
      PersonalSavingsAllowance(150001) shouldBe 0
      generate(150001, Int.MaxValue) map { randomNumber => PersonalSavingsAllowance(randomNumber) shouldBe 0 }
    }

  }

  "SavingsStartingRate" should {

    "be 5000 if payPensionProfitsReceived is less than deductions" in {
      StartingSavingsRate(profitFromSelfEmployments = 5000, totalDeduction = 6000) shouldBe 5000
    }

    "be 5000 if payPensionProfitsReceived is equal to deductions" in {
      StartingSavingsRate(profitFromSelfEmployments = 6000, totalDeduction = 6000) shouldBe 5000
    }

    "be the startingRateLimit - positiveOfZero(totalProfit - totalDeductions)" in {
      StartingSavingsRate(profitFromSelfEmployments = 9000, totalDeduction = 6000) shouldBe 2000
    }

    "return 0 if payPensionProfitsReceived is equal to deductions + startingRateLimit" in {
      StartingSavingsRate(profitFromSelfEmployments = 11000, totalDeduction = 6000) shouldBe 0
    }

    "return 0 if payPensionProfitsReceived is more than deductions + startingRateLimit" in {
      StartingSavingsRate(profitFromSelfEmployments = 12000, totalDeduction = 6000) shouldBe 0
    }
  }

  "run with no deductions" should {

    "calculate tax for total pay pension and profit received lesser than 32000" in {
      PayPensionProfitsTax(totalProfitFromSelfEmployments =  31999, totalDeduction = 0).map(_.amount) should contain theSameElementsAs
        Seq(31999, 0,0)
    }

    "calculate tax for total pay pension and profit received equal to 32000" in {
      PayPensionProfitsTax(totalProfitFromSelfEmployments = 32000, totalDeduction = 0).map(_.amount) should contain theSameElementsInOrderAs
        Seq(32000, 0,0)
    }

    "calculate tax for total pay pension and profit received greater than 32000 but lesser than 150000" in {
      PayPensionProfitsTax(totalProfitFromSelfEmployments = 60000, totalDeduction = 0).map(_.amount) should contain theSameElementsInOrderAs
        Seq(32000, 28000, 0)
    }

    "calculate tax for total pay pension and profit received equal to 150000" in {
      PayPensionProfitsTax(totalProfitFromSelfEmployments = 150000, totalDeduction = 0).map(_.amount) should contain theSameElementsInOrderAs
        Seq(32000, 118000, 0)
    }

    "calculate tax for total pay pension and profit received  greater than 150000" in {
      PayPensionProfitsTax(totalProfitFromSelfEmployments = 300000, totalDeduction = 0).map(_.amount) should contain theSameElementsInOrderAs
        Seq(32000, 118000, 150000)
    }
  }

  "run with deductions" should {

    "calculate tax for total pay pension and profit received lesser than 32000" in {
      PayPensionProfitsTax(totalProfitFromSelfEmployments = 33999, totalDeduction = 2000).map(_.amount) should contain theSameElementsInOrderAs
        Seq(31999, 0, 0)
    }

    "calculate tax for total pay pension and profit received equal to 32000" in {
      PayPensionProfitsTax(totalProfitFromSelfEmployments = 34000, totalDeduction = 2000).map(_.amount) should contain theSameElementsInOrderAs
        Seq(32000, 0, 0)
    }

    "calculate tax for total pay pension and profit received greater than 32000 but lesser than 150000" in {
      PayPensionProfitsTax(totalProfitFromSelfEmployments = 62000, totalDeduction = 2000).map(_.amount) should contain theSameElementsInOrderAs
        Seq(32000, 28000, 0)
    }

    "calculate tax for total pay pension and profit received equal to 150000" in {
      PayPensionProfitsTax(totalProfitFromSelfEmployments = 152000, totalDeduction = 2000).map(_.amount) should contain theSameElementsInOrderAs
        Seq(32000, 118000, 0)
    }

    "calculate tax for total pay pension and profit received  greater than 150000" in {
      PayPensionProfitsTax(totalProfitFromSelfEmployments = 302000, totalDeduction = 2000).map(_.amount) should contain theSameElementsInOrderAs
        Seq(32000, 118000, 150000)
    }

    "calculate tax for total pay pension and profit received lesser than deductions" in {
      PayPensionProfitsTax(totalProfitFromSelfEmployments = 1500, totalDeduction = 2000).map(_.amount) should contain theSameElementsInOrderAs
        Seq(0, 0, 0)
    }

  }

  "run" should {

    "calculate personal dividend allowance when there is no input data" in {
      PersonalDividendAllowance(totalProfitFromSelfEmployments = 0, incomeTaxRelief = 0, personalAllowance = 0,
        totalSavings = 0, totalDividends = 0) shouldBe 0
    }

    "calculate personal dividend allowance capped for various data driven inputs" in {
      val incomeTaxRelief = BigDecimal(2000)
      val personalAllowance = BigDecimal(11000)
      val inputs = Table(
        ("ProfitFromSelfEmployment", "InterestReceived", "DividendIncome", "PersonalDividendAllowance"),
        ("8000", "0", "2000", "0"),
        ("8000", "0", "6000", "1000"),
        ("11000", "2000", "5000", "5000"),
        ("11000", "5000", "5000", "5000"),
        ("13000", "2000", "7000", "5000"),
        ("15000", "3000", "9000", "5000"),
        ("13000", "0", "2000", "2000"),
        ("11000", "0", "2000", "0"),
        ("13000", "0", "5000", "5000"),
        ("14000", "0", "6000", "5000"),
        ("16000", "3000", "8000", "5000")
      )

      TableDrivenPropertyChecks.forAll(inputs) { (profitFromSelfEmployment: String, interestReceived: String, dividendIncome: String, personalDividendAllowance: String) =>
        PersonalDividendAllowance(profitFromSelfEmployment.toInt, incomeTaxRelief, personalAllowance, interestReceived.toInt,
          dividendIncome.toInt) shouldBe personalDividendAllowance.toInt
      }
    }

    "calculate personal dividend allowance when Profit from Self Employments is less than the (Personal Allowance + Income Tax Relief) and there isn't any Savings Income" in {
      PersonalDividendAllowance(totalProfitFromSelfEmployments = 8000, incomeTaxRelief = 4000, personalAllowance = 5000,
        totalSavings = 0, totalDividends = 2000) shouldBe 1000
    }

    "calculate personal dividend allowance when  Profit from Self Employments is less than the (Personal Allowance + Income Tax Relief) and there is a Savings Income (Interest), " +
      "but Sum of Profit from Self Employments and Savings Income is less than the (Personal Allowance + Income Tax Relief)" in {
      PersonalDividendAllowance(totalProfitFromSelfEmployments = 8000, incomeTaxRelief = 4000, personalAllowance = 5000,
        totalSavings = 500, totalDividends = 2000) shouldBe 1500
    }


    "calculate personal dividend allowance when the Profit from Self Employments is less than the (Personal Allowance + Income Tax Relief) and there is a Savings Income (Interest), " +
      "but Sum of Profit from Self Employments and Savings Income is greater or equal to the (Personal Allowance + Income Tax Relief)" in {
      PersonalDividendAllowance(totalProfitFromSelfEmployments = 8000, incomeTaxRelief = 4000, personalAllowance = 5000,
        totalSavings = 1000, totalDividends = 2000) shouldBe 2000
    }


    "calculate personal dividend allowance when the  Profit from Self Employments is greater or equal to the (Personal Allowance + Income Tax Relief) " in {
      PersonalDividendAllowance(totalProfitFromSelfEmployments = 12000, incomeTaxRelief = 4000, personalAllowance = 5000, totalSavings = 0,
        totalDividends = 2000) shouldBe 2000
    }

    "calculate personal dividend allowance capped at 5000 " in {
      PersonalDividendAllowance(totalProfitFromSelfEmployments = 12000, incomeTaxRelief = 4000, personalAllowance = 5000, totalSavings = 0,
        totalDividends = 6000) shouldBe 5000
    }

  }

  "TaxableSavingsIncome" should {
    "be equal to TotalSavingsIncomes - ((PersonalAllowance + IncomeTaxRelief) - ProfitsFromSelfEmployments) if ProfitsFromSelfEmployments < (PersonalAllowance + IncomeTaxRelief) " in {
      TaxableSavingsIncome(totalSavingsIncome = 5000, totalDeduction = 4000, totalProfitFromSelfEmployments = 2000) shouldBe 3000
      TaxableSavingsIncome(totalSavingsIncome = 5000, totalDeduction = 4000, totalProfitFromSelfEmployments = 3999) shouldBe 4999
    }

    "be equal to TotalSavingsIncomes if ProfitsFromSelfEmployments >= (PersonalAllowance + IncomeTaxRelief) " in {
      TaxableSavingsIncome(totalSavingsIncome = 5000, totalDeduction = 4000, totalProfitFromSelfEmployments = 4000) shouldBe 5000
      TaxableSavingsIncome(totalSavingsIncome = 5000, totalDeduction = 4000, totalProfitFromSelfEmployments = 4001) shouldBe 5000
      TaxableSavingsIncome(totalSavingsIncome = 5000, totalDeduction = 4000, totalProfitFromSelfEmployments = 4500) shouldBe 5000
    }
  }

  "SavingsIncomeTax" should {
    case class Print(value: BigDecimal) {
      def as(name: String) = {
        println(s"$name => $value")
        value
      }
    }

    "be allocated to correct tax bands" in {
      val inputs = Table(
        ("TotalProfitFromSelfEmployments", "TotalSavingsIncome", "StartingRateAmount", "NilRateAmount", "BasicRateTaxAmount", "HigherRateTaxAmount", "AdditionalHigherRateAmount"),
        ("8000", "12000", "5000", "1000", "3000", "0", "0"),  //0
        ("5000", "6000", "0", "0", "0", "0", "0"),            //1
        ("5000", "7000", "1000", "0", "0", "0", "0"),         //2
        ("5000", "11000", "5000", "0", "0", "0", "0"),        //3
        ("5000", "12000", "5000", "1000", "0", "0", "0"),        //3
        ("20000", "11000", "0", "1000", "10000", "0", "0"),   //4
        ("29000", "12000", "0", "1000", "11000", "0", "0"),   //5
        ("32000", "12000", "0", "500", "10500", "1000", "0"), //6
        ("100000", "12000", "0", "500", "0", "11500", "0"),   //7
        ("140000", "12000", "0", "0", "0", "10000", "2000"),  //8
        ("150000", "12000", "0", "0", "0", "0", "12000"),     //9
        ("60000", "85000", "0", "500", "0", "84500", "0"),    //10
        ("80000", "85000", "0", "0", "0", "70000", "15000"),
        ("13000", "7000", "3000", "1000", "3000", "0", "0"),
        ("14000", "8000", "2000", "1000", "5000", "0", "0")

      )

      TableDrivenPropertyChecks.forAll(inputs) { (totalProfitFromSelfEmployments: String, totalSavingsIncome: String, startingRateAmount: String,
                                                  nilRateAmount: String, basicRateTaxAmount: String, higherRateTaxAmount: String, additionalHigherRateAmount: String) =>

        val totalIncomeReceived = TotalIncomeReceived(totalProfitFromSelfEmployments = BigDecimal(totalProfitFromSelfEmployments.toInt),
          totalSavings = BigDecimal(totalSavingsIncome.toInt), totalDividends = 0)
        val personalAllowance = Print(PersonalAllowance(totalIncomeReceived)).as("PersonalAllowance")
        val totalDeduction = Print(TotalDeduction(incomeTaxRelief = 0, personalAllowance = personalAllowance)).as("TotalDeductions")
        val savingStartingRate = Print(StartingSavingsRate(profitFromSelfEmployments = totalProfitFromSelfEmployments.toInt,
          totalDeduction = totalDeduction)).as("StartingSavingRate")
        val totalTaxableIncome = Print(TotalTaxableIncome(totalIncomeReceived = totalIncomeReceived,
          totalDeduction = totalDeduction)).as("TotalTaxableIncome")
        val personalSavingsAllowance = Print(PersonalSavingsAllowance(totalTaxableIncome = totalTaxableIncome)).as("PersonalSavingsAllowance")
        val taxableSavingsIncome = Print(TaxableSavingsIncome(totalSavingsIncome = totalSavingsIncome.toInt, totalDeduction = totalDeduction,
          totalProfitFromSelfEmployments = totalProfitFromSelfEmployments.toInt)).as("TaxableSavingsIncome")


        val bandAllocations = SavingsIncomeTax(taxableSavingsIncome = taxableSavingsIncome, startingSavingsRate =
          savingStartingRate,
          personalSavingsAllowance = personalSavingsAllowance, totalTaxableIncome = totalTaxableIncome)

        println(bandAllocations)
        println("==========================================")

        bandAllocations.map(_.amount) shouldBe
            Seq(startingRateAmount.toInt,
              nilRateAmount.toInt,
              basicRateTaxAmount.toInt,
              higherRateTaxAmount.toInt,
              additionalHigherRateAmount.toInt)

      }

    }

  }

}
