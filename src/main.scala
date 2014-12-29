case class Site(v: String)
case class TaxRate(v: String)

trait SiteModule{
 
  type SiteRepository <: SiteRepositoryLike
 
  val siteRepository: SiteRepository
 
  trait SiteRepositoryLike {
 
    this: SiteRepository =>
 
    def getById(id: Option[Int]): Option[Site]
    def search(arg: Option[String], argName: String): Seq[Site]
    def getTaxRates(siteId: Int): Option[Seq[TaxRate]]
    def getNextDailyOrderNumber(siteId: Int): Option[Int]
  }
}
 
trait RedisSiteDao extends SiteModule{
 
  val siteRepository = new SiteRepository()
 
  class SiteRepository extends SiteRepositoryLike{
 
    def getById(id: Option[Int]): Option[Site] = Some(Site("x")) 
    def search(arg: Option[String], argName: String): Seq[Site] = Seq(Site("x")) 
    def getTaxRates(siteId: Int): Option[Seq[TaxRate]] = Some(Seq(TaxRate("a")))
    def getNextDailyOrderNumber(siteId: Int): Option[Int] = Some(11)
  }
}
 
trait AuthenticationModule {
  val m: SiteModule
 
  def doSomething(i: Int) = {
    m.siteRepository.getById(Some(i))
    //Do other things here
  }
}
 
object AuthenticationService extends AuthenticationModule {
    val m = new RedisSiteDao {}

    def handleRequest(i: Int) = {
        doSomething(i)
    }
}


trait AuthenticationComponent{
  type AuthStore
  def authenticationStore: AuthStore
}

trait SiteAuth extends AuthenticationComponent{
  type AuthStore <: SiteAuthStore

  trait SiteAuthStore{
    val a: Int
    def getSiteId(userId: Int): String
  }
}
trait ReportAuth extends AuthenticationComponent{
  type AuthStore <: ReportIdAuthStore

  trait ReportIdAuthStore{
    val b : String
    def getReportId(userId: Int): Int
  }
}
class AuthModule extends SiteAuth with ReportAuth{
  type AuthStore = this.authenticationStore.type

  object authenticationStore extends SiteAuthStore with ReportIdAuthStore{
    val a = 10
    val b = "Rhinoceros"

    def getSiteId(userId: Int) = b
    def getReportId(userId: Int) = a
  }

}