import java.text.SimpleDateFormat

class Aktie(

  // Basisdaten
  val datum: java.util.Date,
  val unternehmen: String,
  val wkn: String,
  val index: String,
  val marktkapitalisierung: Double,
  val aktienkurs:Double,

  // Qualität
  val roe: Double,
  val ebitMarge: Double,
  val ekQuote: Double,

  // Bewertung
  val epsAJ: Double,
  val epsNJ: Double,
  val kgvAktuell: Double,
  val kgv5Jahre: Double,

  // Momentum
  val gewinnrevisionen: Double,
  val kursVs6Monate: Double,
  val kursVs12Monate: Double,
  val momentum: Int,

  // Wachstum
  val gewinnwachstum: Double,

  // Dividendenrendite
  val dividendenrendite: Double,

  // Scorewert
  var score: Int
             ) extends Ordered[Aktie] {

  override def toString(): String = {
    "Basisdaten:"+new SimpleDateFormat("dd.MM.yyyy").format(datum)+" | "+unternehmen+" | "+wkn+" | "+index+" | "+marktkapitalisierung+" | "+aktienkurs+" | "+score+" | "+
    "Qualität (RoE, EBIT, EK): "+roe+" (>20-10<) | "+ebitMarge+" (>12-6<) | "+ekQuote+" (>25-15<) | "+
    "Bewertung (KGV5, KGV): "+kgv5Jahre+" (>12-16<) | "+kgvAktuell+" (>12-16<) | "+
    "Momentum (Gewinnrev, Kurs6, Kurs12, Moment): "+gewinnrevisionen+" | "+kursVs6Monate+" | "+kursVs12Monate+" | "+momentum+" | "+
    "Wachstum (EPS AJ, EPS NJ, Gewinnwachstum): "+epsAJ+" | "+epsNJ+" | "+gewinnwachstum+" | "+
    "Dividendenrendite: "+dividendenrendite+"\n"
  }

  def compare (that: Aktie) = {
    if (this.score == that.score)
      0
    else if (this.score > that.score)
      -1
    else
      1
  }

  def toCsvString(): String = {
    new SimpleDateFormat("dd.MM.yyyy").format(datum)+";"+unternehmen+";"+wkn+";"+index+";"+marktkapitalisierung+";"+aktienkurs+";"+score+";"+
    roe+";"+ebitMarge+";"+ekQuote+";"+
    kgv5Jahre+";"+kgvAktuell+";"+
    gewinnrevisionen+";"+kursVs6Monate+";"+kursVs12Monate+";"+momentum+""+
    epsAJ+";"+epsNJ+";"+gewinnwachstum+";"+
    dividendenrendite+"\n"
  }

}

