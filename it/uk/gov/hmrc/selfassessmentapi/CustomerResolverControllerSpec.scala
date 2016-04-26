package uk.gov.hmrc.selfassessmentapi

import uk.gov.hmrc.support.BaseFunctionalSpec

class CustomerResolverControllerSpec extends BaseFunctionalSpec {

  val saUtr = generateSaUtr()

  "Customer Resolver" should {
    "return a 200 response with a link to /self-assessment/utr when the customer is enrolled in SA" in {
      given().userIsEnrolledInSa(saUtr)
        .when()
        .get("/")
        .thenAssertThat()
        .statusIs(200)
        .contentTypeIs("application/hal+json")
        .bodyHasLink("self-assessment", s"/self-assessment/$saUtr")
    }

    "return a 401 response the customer is not enrolled in SA" in {
      given().userIsNotEnrolledInSa
        .when()
        .get("/")
        .thenAssertThat()
        .statusIs(401)
    }
  }

}