package example
package models

case class SearchTarget(
                         orgs: List[Organization],
                         users: List[User],
                         tickets: List[Ticket]
                       )