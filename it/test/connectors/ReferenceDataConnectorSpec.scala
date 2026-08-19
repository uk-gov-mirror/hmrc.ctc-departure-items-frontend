/*
 * Copyright 2024 HM Revenue & Customs
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

package connectors

import cats.data.NonEmptySet
import com.github.tomakehurst.wiremock.client.WireMock.*
import connectors.ReferenceDataConnector.NoReferenceDataFoundException
import itbase.{ItSpecBase, WireMockServerHandler}
import models.reference.*
import models.{DeclarationTypeItemLevel, PackingType}
import org.scalacheck.Gen
import org.scalatest.{Assertion, EitherValues}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.guice.GuiceApplicationBuilder

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ReferenceDataConnectorSpec extends ItSpecBase with WireMockServerHandler with ScalaCheckPropertyChecks with EitherValues {

  private val baseUrl                                = "customs-reference-data/test-only"
  private lazy val connector: ReferenceDataConnector = app.injector.instanceOf[ReferenceDataConnector]

  override def guiceApplicationBuilder(): GuiceApplicationBuilder = super
    .guiceApplicationBuilder()
    .configure(
      conf = "microservice.services.customs-reference-data.port" -> server.port()
    )

  private val countriesResponseJson: String =
    s"""
       |[
       |    {
       |      "key": "GB",
       |      "value": "United Kingdom"
       |    },
       |    {
       |      "key": "AD",
       |      "value": "Andorra"
       |    }
       |]
       |""".stripMargin

  private val countryResponseJson: String =
    s"""
       |[
       |    {
       |      "key": "GB",
       |      "value": "United Kingdom"
       |    }
       |]
       |""".stripMargin

  private val packageTypeJson: String =
    s"""
      |[
      | {
      |    "key": "VA",
      |    "value": "Vat"
      |  },
      |  {
      |    "key": "UC",
      |    "value": "Uncaged"
      |  }
      |]
      |""".stripMargin

  private val additionalReferenceJson: String =
    """
      |[
      | {
      |    "key": "documentType1",
      |    "value": "desc1"
      |  },
      |  {
      |    "key": "documentType2",
      |    "value": "desc2"
      |  }
      |]
      |""".stripMargin

  private val additionalInformationJson: String =
    """
      |[
      | {
      |    "key": "additionalInfoCode1",
      |    "value": "additionalInfoDesc1"
      |  },
      |  {
      |    "key": "additionalInfoCode2",
      |    "value": "additionalInfoDesc2"
      |  }
      |]
      |""".stripMargin

  private val methodOfPaymentJson: String =
    """
      |[
      | {
      |    "key": "A",
      |    "value": "Payment By Card"
      |  },
      |  {
      |    "key": "B",
      |    "value": "PayPal"
      |  }
      |]
      |""".stripMargin

  private val declarationTypesResponseJson: String =
    """
      |[
      |    {
      |      "key": "T2",
      |      "value": "Goods having the customs status of Union goods, which are placed under the common transit procedure"
      |    },
      |    {
      |      "key": "TIR",
      |      "value": "TIR carnet"
      |    }
      |]
      |""".stripMargin

  val supplyChainActorTypesResponseJson: String =
    """
      |[
      |    {
      |      "key":"CS",
      |      "value":"Consolidator"
      |    },
      |    {
      |      "key":"MF",
      |      "value":"Manufacturer"
      |    }
      |]
      |""".stripMargin

  private val documentTypeExciseJson: String =
    """
      |[
      |  {
      |    "key": "C651",
      |    "value": "AAD - Administrative Accompanying Document (EMCS)"
      |  },
      |  {
      |    "key": "C658",
      |    "value": "FAD - Fallback e-AD (EMCS)"
      |  }
      |]
      |""".stripMargin

  private val emptyResponseJson: String =
    """
      |[]
      |""".stripMargin

  "Reference Data" - {

    "getCUSCode" - {

      val cusCode = "0010001-6"
      val url     = s"/$baseUrl/lists/CUSCode?keys=$cusCode"

      val json: String =
        """
          |[
          |    {
          |      "key": "0010001-6"
          |    }
          |]
          |""".stripMargin

      "must return CUSCode when successful" in {

        server.stubFor(
          get(urlEqualTo(url))
            .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
            .willReturn(okJson(json))
        )

        val expectedResult = CUSCode(cusCode)

        connector.getCUSCode(cusCode).futureValue.value mustEqual expectedResult
      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        val connector = app.injector.instanceOf[ReferenceDataConnector]
        checkNoReferenceDataFoundResponse(url, emptyResponseJson, connector.getCUSCode(cusCode))
      }

      "must return an exception when an error response is returned" in {
        val connector = app.injector.instanceOf[ReferenceDataConnector]
        checkErrorResponse(url, connector.getCUSCode(cusCode))
      }

    }

    "getHSCode" - {

      val code = "010121"
      val url  = s"/$baseUrl/lists/HScode?keys=$code"

      val json: String =
        """
          |[
          |    {
          |      "key": "010121"
          |    }
          |]
          |""".stripMargin

      "must return HSCode when successful" in {
        server.stubFor(
          get(urlEqualTo(url))
            .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
            .willReturn(okJson(json))
        )

        val expectedResult = HSCode(code)

        connector.getHSCode(code).futureValue.value mustEqual expectedResult
      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        checkNoReferenceDataFoundResponse(url, emptyResponseJson, connector.getHSCode(code))
      }

      "must return an exception when an error response is returned" in {
        checkErrorResponse(url, connector.getHSCode(code))
      }
    }

    "getDocumentTypeExcise" - {
      val code = "C651"

      val url = s"/$baseUrl/lists/DocumentTypeExcise?keys=$code"

      "must return DocumentTypeExcise when successful" in {
        server.stubFor(
          get(urlEqualTo(url))
            .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
            .willReturn(okJson(documentTypeExciseJson))
        )

        val expectedResult =
          DocTypeExcise(code = "C651", description = "AAD - Administrative Accompanying Document (EMCS)")

        connector.getDocumentTypeExcise(code).futureValue.value mustEqual expectedResult
      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        checkNoReferenceDataFoundResponse(url, emptyResponseJson, connector.getDocumentTypeExcise(code))
      }

      "must return an exception when an error response is returned" in {
        checkErrorResponse(url, connector.getDocumentTypeExcise(code))
      }
    }

    "getDeclarationTypeItemLevel" - {
      val url = s"/$baseUrl/lists/DeclarationTypeItemLevel"
      "must return Seq of declaration types when successful" in {
        server.stubFor(
          get(urlEqualTo(url))
            .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
            .willReturn(okJson(declarationTypesResponseJson))
        )

        val expectedResult = NonEmptySet.of(
          DeclarationTypeItemLevel("T2", "Goods having the customs status of Union goods, which are placed under the common transit procedure"),
          DeclarationTypeItemLevel("TIR", "TIR carnet")
        )

        val res = connector.getDeclarationTypeItemLevel().futureValue.value

        res mustEqual expectedResult
      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        checkNoReferenceDataFoundResponse(url, emptyResponseJson, connector.getDeclarationTypeItemLevel())
      }

      "must return an exception when an error response is returned" in {
        checkErrorResponse(url, connector.getDeclarationTypeItemLevel())
      }
    }

    "getCountries" - {
      val url = s"/$baseUrl/lists/CountryCodesFullList"

      "must return Seq of Country when successful" in {
        server.stubFor(
          get(urlEqualTo(url))
            .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
            .willReturn(okJson(countriesResponseJson))
        )

        val expectedResult = NonEmptySet.of(
          Country(CountryCode("GB"), "United Kingdom"),
          Country(CountryCode("AD"), "Andorra")
        )

        connector.getCountries().futureValue.value mustEqual expectedResult
      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        checkNoReferenceDataFoundResponse(url, emptyResponseJson, connector.getCountries())
      }

      "must return an exception when an error response is returned" in {
        checkErrorResponse(url, connector.getCountries())
      }

    }

    "getCountryCodesForAddress" - {
      val url = s"/$baseUrl/lists/CountryCodesForAddress"

      "must return Seq of Country when successful" in {
        server.stubFor(
          get(urlEqualTo(url))
            .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
            .willReturn(okJson(countriesResponseJson))
        )

        val expectedResult = NonEmptySet.of(
          Country(CountryCode("GB"), "United Kingdom"),
          Country(CountryCode("AD"), "Andorra")
        )

        connector.getCountryCodesForAddress().futureValue.value mustEqual expectedResult
      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        checkNoReferenceDataFoundResponse(url, emptyResponseJson, connector.getCountryCodesForAddress())
      }

      "must return an exception when an error response is returned" in {
        checkErrorResponse(url, connector.getCountryCodesForAddress())
      }
    }

    "getCountryCodeCommonTransit" - {

      def url(code: String) = s"/$baseUrl/lists/CountryCodesCommonTransit?keys=$code"

      "must return Country when successful" in {
        val code = "GB"
        server.stubFor(
          get(urlEqualTo(url(code)))
            .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
            .willReturn(okJson(countryResponseJson))
        )

        val country = Country(CountryCode(code), "United Kingdom")

        connector.getCountryCodeCommonTransit(country).futureValue.value mustEqual country

      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        val code    = "FR"
        val country = Country(CountryCode(code), "France")

        checkNoReferenceDataFoundResponse(url(code), emptyResponseJson, connector.getCountryCodeCommonTransit(country))
      }

      "must return an exception when an error response is returned" in {
        val code    = "GB"
        val country = Country(CountryCode(code), "United Kingdom")
        checkErrorResponse(url(code), connector.getCountryCodeCommonTransit(country))
      }
    }

    "getCountriesWithoutZipCountry" - {

      def url(countryId: String) = s"/$baseUrl/lists/CountryWithoutZip?keys=$countryId"

      "must return Seq of Country when successful" in {
        val countryId = "GB"
        server.stubFor(
          get(urlEqualTo(url(countryId)))
            .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
            .willReturn(okJson(countryResponseJson))
        )

        val expectedResult = CountryCode(countryId)

        connector.getCountriesWithoutZipCountry(countryId).futureValue.value mustEqual expectedResult
      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        val countryId = "FR"
        checkNoReferenceDataFoundResponse(url(countryId), emptyResponseJson, connector.getCountriesWithoutZipCountry(countryId))
      }

      "must return an exception when an error response is returned" in {
        val countryId = "FR"
        checkErrorResponse(url(countryId), connector.getCountriesWithoutZipCountry(countryId))
      }
    }

    "getPackageTypes" - {
      val url = s"/$baseUrl/lists/KindOfPackages"

      "must return list of package types when successful" in {
        server.stubFor(
          get(urlEqualTo(url))
            .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
            .willReturn(okJson(packageTypeJson))
        )

        val expectResult = NonEmptySet.of(
          PackageType("VA", "Vat", PackingType.Other),
          PackageType("UC", "Uncaged", PackingType.Other)
        )

        connector.getPackageTypes().futureValue.value mustEqual expectResult
      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        checkNoReferenceDataFoundResponse(url, emptyResponseJson, connector.getPackageTypes())
      }

      "must return an exception when an error response is returned" in {
        checkErrorResponse(url, connector.getPackageTypes())
      }

    }

    "getPackageTypesBulk" - {
      val url = s"/$baseUrl/lists/KindOfPackagesBulk"

      "must return list of package types when successful" in {
        server.stubFor(
          get(urlEqualTo(url))
            .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
            .willReturn(okJson(packageTypeJson))
        )

        val expectResult = NonEmptySet.of(
          PackageType("VA", "Vat", PackingType.Bulk),
          PackageType("UC", "Uncaged", PackingType.Bulk)
        )

        connector.getPackageTypesBulk().futureValue.value mustEqual expectResult
      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        checkNoReferenceDataFoundResponse(url, emptyResponseJson, connector.getPackageTypesBulk())
      }

      "must return an exception when an error response is returned" in {
        checkErrorResponse(url, connector.getPackageTypesBulk())
      }
    }

    "getPackageTypesUnpacked" - {
      val url = s"/$baseUrl/lists/KindOfPackagesUnpacked"
      "must return list of package types when successful" in {
        server.stubFor(
          get(urlEqualTo(url))
            .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
            .willReturn(okJson(packageTypeJson))
        )

        val expectResult = NonEmptySet.of(
          PackageType("VA", "Vat", PackingType.Unpacked),
          PackageType("UC", "Uncaged", PackingType.Unpacked)
        )

        connector.getPackageTypesUnpacked().futureValue.value mustEqual expectResult
      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        checkNoReferenceDataFoundResponse(url, emptyResponseJson, connector.getPackageTypesUnpacked())
      }

      "must return an exception when an error response is returned" in {
        checkErrorResponse(url, connector.getPackageTypesUnpacked())
      }
    }

    "getAdditionalReferences" - {
      val url = s"/$baseUrl/lists/AdditionalReference"

      "must return Seq of AdditionalReference when successful" in {

        server.stubFor(
          get(urlEqualTo(url))
            .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
            .willReturn(okJson(additionalReferenceJson))
        )

        val expectedResult = NonEmptySet.of(
          AdditionalReference("documentType1", "desc1"),
          AdditionalReference("documentType2", "desc2")
        )

        connector.getAdditionalReferences().futureValue.value mustEqual expectedResult
      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        checkNoReferenceDataFoundResponse(url, emptyResponseJson, connector.getAdditionalReferences())
      }

      "must return an exception when an error response is returned" in {
        checkErrorResponse(url, connector.getAdditionalReferences())
      }
    }

    "getAdditionalInformationTypes" - {
      val url = s"/$baseUrl/lists/AdditionalInformation"

      "must return list of additional information types when successful" in {
        server.stubFor(
          get(urlEqualTo(url))
            .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
            .willReturn(okJson(additionalInformationJson))
        )

        val expectResult = NonEmptySet.of(
          AdditionalInformation("additionalInfoCode1", "additionalInfoDesc1"),
          AdditionalInformation("additionalInfoCode2", "additionalInfoDesc2")
        )

        connector.getAdditionalInformationTypes().futureValue.value mustEqual expectResult

      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        checkNoReferenceDataFoundResponse(url, emptyResponseJson, connector.getAdditionalInformationTypes())
      }

      "must return an exception when an error response is returned" in {
        checkErrorResponse(url, connector.getAdditionalInformationTypes())
      }
    }

    "getMethodOfPaymentTypes" - {
      val url = s"/$baseUrl/lists/TransportChargesMethodOfPayment"

      "must return Seq of MethodOfPayments when successful" in {
        server.stubFor(
          get(urlEqualTo(url))
            .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
            .willReturn(okJson(methodOfPaymentJson))
        )

        val expectedResult = NonEmptySet.of(
          TransportChargesMethodOfPayment("A", "Payment By Card"),
          TransportChargesMethodOfPayment("B", "PayPal")
        )

        connector.getTransportChargesMethodOfPaymentTypes().futureValue.value mustEqual expectedResult
      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        checkNoReferenceDataFoundResponse(url, emptyResponseJson, connector.getTransportChargesMethodOfPaymentTypes())
      }

      "must return an exception when an error response is returned" in {
        checkErrorResponse(url, connector.getTransportChargesMethodOfPaymentTypes())
      }
    }

    "getSupplyChainActorTypes" - {
      val url: String = s"/$baseUrl/lists/AdditionalSupplyChainActorRoleCode"

      "must return Seq of SupplyChainActorType when successful" in {
        server.stubFor(
          get(urlEqualTo(url))
            .withHeader("Accept", equalTo("application/vnd.hmrc.2.0+json"))
            .willReturn(okJson(supplyChainActorTypesResponseJson))
        )

        val expectedResult = NonEmptySet.of(
          SupplyChainActorType("CS", "Consolidator"),
          SupplyChainActorType("MF", "Manufacturer")
        )

        connector.getSupplyChainActorTypes().futureValue.value mustEqual expectedResult
      }

      "must throw a NoReferenceDataFoundException for an empty response" in {
        checkNoReferenceDataFoundResponse(url, emptyResponseJson, connector.getSupplyChainActorTypes())
      }

      "must return an exception when an error response is returned" in {
        checkErrorResponse(url, connector.getSupplyChainActorTypes())
      }
    }

    def checkNoReferenceDataFoundResponse(url: String, json: String, result: => Future[Either[Exception, ?]]): Assertion = {
      server.stubFor(
        get(urlEqualTo(url))
          .willReturn(okJson(json))
      )

      result.futureValue.left.value mustBe a[NoReferenceDataFoundException]
    }

    def checkErrorResponse(url: String, result: => Future[Either[Exception, ?]]): Assertion = {
      val errorResponses: Gen[Int] = Gen.chooseNum(400: Int, 599: Int)

      forAll(errorResponses) {
        errorResponse =>
          server.stubFor(
            get(urlEqualTo(url))
              .willReturn(
                aResponse()
                  .withStatus(errorResponse)
              )
          )
          result.futureValue.left.value mustBe an[Exception]
      }
    }
  }
}
