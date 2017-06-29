package nlp100

object s1 {

  /**
    * 00. 文字列の逆順
    * 文字列"stressed"の文字を逆に（末尾から先頭に向かって）並べた文字列を得よ．
    */
  val p00: String = "stressed".reverse

  /**
    * 01. 「パタトクカシーー」
    * 「パタトクカシーー」という文字列の1,3,5,7文字目を取り出して連結した文字列を得よ．
    */
  val p01: String =
    "パタトクカシーー".zipWithIndex.filter(p => p._2 % 2 == 0).map(p => p._1).foldLeft("")((z, n) => z + n)

  /**
    * 02. 「パトカー」＋「タクシー」＝「パタトクカシーー」
    * 「パトカー」＋「タクシー」の文字を先頭から交互に連結して文字列「パタトクカシーー」を得よ．
    */
  val p02: String = (0 to 3).toList.foldLeft("")((z, n) => z + "パトカー" (n) + "タクシー" (n))

  /**
    * 03. 円周率
    * "Now I need a drink, alcoholic of course, after the heavy lectures involving quantum mechanics."という文を単語に分解し，各単語の（アルファベットの）文字数を先頭から出現順に並べたリストを作成せよ．
    */
  val p03: List[Int] =
    "Now I need a drink, alcoholic of course, after the heavy lectures involving quantum mechanics."
      .replace(",", "")
      .replace(".", "")
      .split(" ")
      .toList
      .map(x => x.length)

  /**
    * 04. 元素記号
    * "Hi He Lied Because Boron Could Not Oxidize Fluorine. New Nations Might Also Sign Peace Security Clause. Arthur King Can."という文を単語に分解し，1, 5, 6, 7, 8, 9, 15, 16, 19番目の単語は先頭の1文字，それ以外の単語は先頭に2文字を取り出し，取り出した文字列から単語の位置（先頭から何番目の単語か）への連想配列（辞書型もしくはマップ型）を作成せよ．
    */
  val p04: Map[Any, Int] =
    "Hi He Lied Because Boron Could Not Oxidize Fluorine. New Nations Might Also Sign Peace Security Clause. Arthur King Can."
      .replace(",", "")
      .replace(".", "")
      .split(" ")
      .toList
      .zipWithIndex
      .toMap
      .map {
        case (str, idx) if List(1, 5, 6, 7, 8, 9, 15, 16, 19).exists(i => i - 1 - idx == 0) =>
          (str.head, idx + 1)
        case (str, idx) => (str.take(2), idx + 1)
      }

  /**
    * 05. n-gram
    * 与えられたシーケンス（文字列やリストなど）からn-gramを作る関数を作成せよ．この関数を用い，"I am an NLPer"という文から単語bi-gram，文字bi-gramを得よ．
    */
  def p05(seq: Seq[Char] = "I am an NLPer"): (List[(String, String)], List[String]) = {
    def word(str: List[String], acc: List[(String, String)] = Nil): List[(String, String)] =
      (str, acc) match {
        case (Nil, res)    => res.reverse
        case (h :: Nil, l) => word(Nil, l)
        case (h :: t, l)   => word(t, (h, t.head) :: l)
      }
    def char(str: String, acc: List[String] = Nil): List[String] = (str, acc) match {
      case ("", res)               => res.reverse
      case (s, l) if s.length == 1 => char("", l)
      case (s, l)                  => char(s.tail, s.take(2) :: l)
    }
    (word(seq.toString.split(" ").toList), char(seq.toString))
  }

  /**
    * 06. 集合
    * "paraparaparadise"と"paragraph"に含まれる文字bi-gramの集合を，それぞれ, XとYとして求め，XとYの和集合，積集合，差集合を求めよ．さらに，'se'というbi-gramがXおよびYに含まれるかどうかを調べよ．
    */
  val p06: (Set[String], Set[String], Set[String], Set[String], Set[String], Boolean, Boolean) = {
    val strX      = "paraparaparadise"
    val strY      = "paragraph"
    val X         = p05(strX)._2.toSet
    val Y         = p05(strY)._2.toSet
    val union     = X | Y
    val intersect = X & Y
    val diff      = X &~ Y
    (X, Y, union, intersect, diff, X.contains("se"), Y.contains("se"))
  }

  /**
    * 07. テンプレートによる文生成
    * 引数x, y, zを受け取り「x時のyはz」という文字列を返す関数を実装せよ．さらに，x=12, y="気温", z=22.4として，実行結果を確認せよ．
    */
  def p07(x: Any = 12, y: Any = "気温", z: Any = 22.4): String = {
    s"${x}時の${y}は${z}"
  }

  /**
    * 08. 暗号文
    *
    * 与えられた文字列の各文字を，以下の仕様で変換する関数cipherを実装せよ．
    *
    *     英小文字ならば(219 - 文字コード)の文字に置換
    *     その他の文字はそのまま出力
    *
    * この関数を用い，英語のメッセージを暗号化・復号化せよ．
    */
  def cipher(s: String): String = {
    s.toSeq
      .map(x =>
        x.toInt match {
          case i if i >= 97 && i <= 122 => (219 - i).toChar
          case i                        => i.toChar
      })
      .foldLeft("")((z, n) => z + n)
  }

  /**
    * 09. Typoglycemia
    * スペースで区切られた単語列に対して，各単語の先頭と末尾の文字は残し，それ以外の文字の順序をランダムに並び替えるプログラムを作成せよ．ただし，長さが４以下の単語は並び替えないこととする．適当な英語の文（例えば"I couldn't believe that I could actually understand what I was reading : the phenomenal power of the human mind ."）を与え，その実行結果を確認せよ．
    */
  def p09(s: String =
    "I couldn't believe that I could actually understand what I was reading : the phenomenal power of the human mind .")
    : String = {
    s.split(" ")
      .toList
      .map {
        case w if w.length <= 4 => w
        case w =>
          w.head +
            scala.util.Random
              .shuffle(w.toSeq.tail.init)
              .foldLeft("")((z, n) => z + n) + w.last
      }
      .reduce((z, n) => z + " " + n)
  }
}
