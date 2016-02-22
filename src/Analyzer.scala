import java.text.SimpleDateFormat
import java.util.Calendar

import scala.collection.mutable.ListBuffer

object Analyzer {
  def main (args: Array[String]) {
    val aktienListe: List[Aktie] = readCSV(args(0), args(1))
    //val scoredAktienListe: List[Aktie] = calculateScore(aktienListe)
    //displayScoredShares(scoredAktienListe)

    val altScoredAktienListe: List[Aktie] = calculateAlternativeScore(aktienListe)
    displayScoredShares(altScoredAktienListe)
  }

  val meineAktien: List[String] = List(
  "547030", "884437", "508903", "511170", "851311",
  "552484", "A0EQ57", "701080", "A1JRLK", "885823",
  "922230", "A110NH", "593397", "DK1A47", "A0NFN3")

  val positivlisteEthikbank: List[String] = List(
    "633500", "540811", "A1H8BV", "A0CAYB", "623100",
    "LED400", "840221", "581005", "843002")

  val watchlist: List[String] = List(
    "608343", "621993", "747206", "540750", "723133",
    "609500", "633500"
  )

  val nx25: List[String] = List(
    "A0WMPJ", "540750", "567710", "720190", "589730",
    "A0HNG5", "A0JQ5U", "A0BVU9", "A0JBPG", "A0Z1JH"
  )

  val familienUnternehmen: List[String] = List(
    "519003", "543730", "547030", "577220", "580060",
    "548810", "633500", "723132", "508903"
  )


  def readCSV(date: String, pathToFile: String): List[Aktie] = {
    val bufferedSource = io.Source.fromFile(pathToFile, "ISO-8859-1")

    val aktienListBuffer: ListBuffer[Aktie] = new ListBuffer[Aktie]()

    for (line <- bufferedSource.getLines()) {
      val cols = line.split(";").map(_.trim)

      // Basisdaten
      val datum: java.util.Date = new SimpleDateFormat("dd.MM.yyyy").parse(date)
      val unternehmen = cols(0)
      val wkn = cols(2)
      val index = cols(5)
      val marktkapitalisierung = parseDouble(cols(19))
      val aktienkurs = parseDouble(cols(7))

      // Qualität
      val roe: Double = parseDouble(cols(30))
      val ebitMarge = parseDouble(cols(32))
      val ekQuote = parseDouble(cols(29))

      // Bewertung
      val epsAJ = parseDouble(cols(62))
      val epsNJ = parseDouble(cols(63))

      val kgvAktuell = if(epsAJ == 0) aktienkurs else roundDouble(aktienkurs / epsAJ)
      val kgv5Jahre = roundDouble(aktienkurs / ((parseDouble(cols(59)) + parseDouble(cols(60)) + parseDouble(cols(61)) + epsAJ + epsNJ) / 5))

      // Momentum
      val gewinnrevisionen = 0
      val kursVs6Monate = parseDouble(cols(11))
      val kursVs12Monate = parseDouble(cols(13))
      val momentum = if(kursVs6Monate > 5 && kursVs12Monate < 5) 1
        else if(kursVs6Monate < -5 && kursVs12Monate > -5) -1
        else 0

      // Wachstum
      val gewinnwachstum = roundDouble((1 - (epsAJ / epsNJ)) * 100)

      // Dividendenrendite
      val dividendenrendite = parseDouble(cols(24))

      val score = 0

      val aktie: Aktie = new Aktie(datum, unternehmen, wkn, index, marktkapitalisierung, aktienkurs, roe, ebitMarge, ekQuote, epsAJ, epsNJ, kgvAktuell, kgv5Jahre, gewinnrevisionen, kursVs6Monate, kursVs12Monate, momentum, gewinnwachstum, dividendenrendite, score)
      //println(aktie)

      aktienListBuffer += aktie
    }

    bufferedSource.close()
    aktienListBuffer.toList
  }


  def calculateScore(aktienListe: List[Aktie]): List[Aktie] = {
    val aktienListBuffer: ListBuffer[Aktie] = new ListBuffer[Aktie]()

    for (aktie <- aktienListe) {
      var score = 0

      if(aktie.roe > 20)
        score += 1
      if(aktie.roe < 10)
        score -= 1
      if(aktie.ebitMarge > 12)
        score += 1
      if(aktie.ebitMarge < 6)
        score -= 1
      if(aktie.ekQuote > 25)
        score += 1
      if(aktie.ekQuote < 15)
        score -= 1
      if(aktie.kgv5Jahre < 12)
        score += 1
      if(aktie.kgv5Jahre > 16)
        score -= 1
      if(aktie.kgvAktuell < 12)
        score += 1
      if(aktie.kgvAktuell > 16)
        score -= 1
      if(aktie.kursVs6Monate > 5)
        score += 1
      if(aktie.kursVs6Monate < -5)
        score -= 1
      if(aktie.kursVs12Monate > 5)
        score += 1
      if(aktie.kursVs12Monate < -5)
        score -= 1
      if(aktie.gewinnwachstum > 5)
        score += 1
      if(aktie.gewinnwachstum < -5)
        score -= 1

      score += aktie.momentum

      aktie.score = score

      aktienListBuffer += aktie
    }

    aktienListBuffer.toList
   }

  def calculateAlternativeScore(aktienListe: List[Aktie]): List[Aktie] = {
    val aktienListBuffer: ListBuffer[Aktie] = new ListBuffer[Aktie]()

    for (aktie <- aktienListe) {
      var score = 0

      if(aktie.roe > 20)
        score += 1
      if(aktie.roe < 10)
        score -= 1
      if(aktie.ebitMarge > 12)
        score += 1
      if(aktie.ebitMarge < 6)
        score -= 1
      if(aktie.ekQuote > 25)
        score += 1
      if(aktie.ekQuote < 15)
        score -= 1
      if(aktie.kgv5Jahre < 12)
        score += 1
      if(aktie.kgv5Jahre > 16)
        score -= 1
      if(aktie.kgvAktuell < 12)
        score += 1
      if(aktie.kgvAktuell > 16)
        score -= 1
      if(aktie.gewinnwachstum > 5)
        score += 1
      if(aktie.gewinnwachstum < -5)
        score -= 1
      if(aktie.dividendenrendite > 3)
        score += 1
      if(aktie.dividendenrendite < 1)
        score -= 1

      aktie.score = score

      aktienListBuffer += aktie
    }

    aktienListBuffer.toList
  }

  def displayScoredShares(aktienListe: List[Aktie]): Unit = {

    // Aktien mit Scorewert >= 4
    //aktienListe.filter(p => p.score >= 4).sorted.map(f => print(f))

    // Aktien mit Scorewert >= 4 die nicht im TecDAX sind
    //aktienListe.filter(p => p.score >= 4 && !p.index.equals("TecDAX")).sorted.map(f => print(f))

    // Positivliste Ethikbank
    //aktienListe.filter(p => positivlisteEthikbank.contains(p.wkn)).sorted.map(f => print(f))
    //aktienListe.filter(p => nx25.contains(p.wkn)).sorted.map(f => print(f))
    //aktienListe.filter(p => familienUnternehmen.contains(p.wkn)).sorted.map(f => print(f))

    // Meine Aktien
    //aktienListe.filter(p => meineAktien.contains(p.wkn)).map(f => print(f))
    //aktienListe.filter(p => meineAktien.contains(p.wkn)).sorted.map(f => print(f.toCsvString()))

    //aktienListe.filter(p => watchlist.contains(p.wkn)).sorted.map(f => print(f))

    // Einzelwerte überprüfen
    aktienListe.filter(p => p.wkn.equals("587530")).map(f => print(f))

  }

  def parseDouble(s: String): Double = {
    try {
      val d: Double = s.toDouble
      return roundDouble(d)
    } catch {
      case _ => 0
    }
  }

  def roundDouble(d: Double): Double = {
    BigDecimal(d).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
  }
}
