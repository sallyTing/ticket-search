package example.models

trait Searchable[A] {
  def search(a: A, keyword: String): Boolean
  def parseFromJsonString(jsonString: String): Either[Throwable, List[A]]
}

object Searchable {
  def search[A](a: A, keyword: String)(implicit searchable: Searchable[A]) = searchable.search(a, keyword)
  def parseFromJsonString[A](jsonString: String)(implicit searchable: Searchable[A]): Either[Throwable, List[A]] =
    searchable.parseFromJsonString(jsonString)
}