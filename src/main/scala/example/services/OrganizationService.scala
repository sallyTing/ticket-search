package example
package services

object OrganizationService {

  def getAllOrganizations() =
    DBConnector.getAllOrganizations()

  def getOrganizationById(id: Int): Unit = {

  }

}
