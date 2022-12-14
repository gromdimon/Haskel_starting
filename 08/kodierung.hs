-- Informatik A WiSe 20/21
-- 04.01.2021
-- Max Willert
module Kodierung where
import Data.Char 

-- Cäsar-Kodierung =====================================================
-- Damit niemand anderes die Botschaften lesen kann, die Cäsar mit seinen
-- Legionären austauscht, werden die Buchstaben des Textes immer um genau
-- drei Stellen im Alphabet (zyklisch) verschoben.
-- caesar "alea iacta est" liefert z.B. "dohd ldfwd hvw"
-- Natürlich kann man das ganz Verfahren verallgemeinern und nicht um 3
-- sondern um n Stellen verschieben.

alpha = ['a'..'z'] -- Das Alphabet der Kleinbuchstaben

-- Vor.: Eingabe-Char ist ein Kleinbuchstabe
-- Erg.: n-te Cäsarkodierung des Eingabechars ist geliefert.
kodiereChar :: Int -> Char -> Char
kodiereChar n c
   | isLower c = chr (ord 'a' + (ord c - ord 'a' + n)`mod`26)
   | otherwise = c

-- Vor.: keine
-- Erg.: String ist geliefert, in dem alle Kleinbuchstaben durch ihre
--       n-te Cäsarkodierung ersetzt sind.
kodiere :: Int -> String -> String
kodiere n cs = map (kodiereChar n) cs

-- Vor.: keine
-- Erg.: String ist geliefert, in dem alle Kleinbuchstaben durch ihre
--       3. Cäsarkodierung ersetzt sind.
caesar :: String -> String
caesar = kodiere 3

-- Vor.: keine
-- Erg.: String ist geliefert, in dem alle Kleinbuchstaben durch ihre
--       2. Cäsarkodierung ersetzt sind.
brutus :: String -> String
brutus = kodiere 2

-- Vor.: keine
-- Erg.: String ist geliefert, in dem alle Kleinbuchstaben durch ihre
--       n-te CäsarDEkodierung ersetzt sind.
dekodiere :: Int -> String -> String
dekodiere n = kodiere (-n)

-- Histogramme =========================================================
-- Um die allgemeine Cäsar-Kodierung knacken zu können (d.h. herauszu-
-- finden, mit welchem n ein Text verschlüsselt wurde), muss man heraus-
-- finden, welcher Buchstabe am häufigsten vorkommt. In einem deutschen
-- Text wurde höchstwahrscheinlich 'e' auf diesen Buchstaben abgebildet.
-- Der Abstand des Buchstaben zum 'e' entspricht vermutlich dem Schlüssel n.

-- Zentrales Hilfsmittel hierbei ist das sogenannte Histogramm eines
-- Textes: Eine Tabelle, bei der jedem Buchstaben (Char) seine absolute
-- Häufigkeit im Text (Int) zugeordnet ist.

-- Aus Gründen der Lesbarkeit definieren wir dazu zwei Typen: Eine Tabelle
-- ist eine endliche Folge von Einträgen und ein Eintrag entspricht genau
-- einer Zeile in der Tabelle: also einem Char und dessen zugehörige
-- absolute Häufigkeit (Int).

type Eintrag = (Char,Int)
type Tabelle = [Eintrag]

-- Vor.: keine
-- Erg.: Ein Histogramm des Eingabestrings ist geliefert.
histogramm :: String -> Tabelle
histogramm [] = []
-- sortiere den Char c in das rekursiv berechnete Histogramm vom Rest ein
histogramm (c:cs) = einsortieren c (histogramm cs) where
    einsortieren c [] = [(c,1)] -- c ist noch nicht vorhanden, neu erstellen!
    einsortieren c ((d,i):dis)
      | c == d = (d,i+1) : dis -- c wurde gefunden, Counter erhöhen
      | otherwise = (d,i) : einsortieren c dis -- c wurde noch nicht gefunden

-- =====================================================================
-- Wir wollen nun noch das Histogramm eines Textes auf dem Bildschirm anzeigen.

-- Die Funktion formatiere wandelt eine Tabelle in einen String um.
formatiere :: Tabelle -> String
formatiere [] = []
formatiere ((c,i):cis) = [c] ++ ":\t" ++ show i ++ "\n" ++ 
                        formatiere cis

-- Damit wir nun das Histogramm eines Strings auf dem Bildschirm ANZEIGEN
-- können, benötigen wir noch die von Haskell zur verfügung gestellte Funktion
-- putStr :: String -> IO() Sie kann einen String auf schöne Art und Weise
-- auf unserem Terminal anzeigen. Hier kommt unser Ergebnis:

zeigeHistogramm :: String -> IO()
zeigeHistogramm cs = (putStr.formatiere.histogramm) cs

-- Zum Testen können Sie in GHCi bzw. WinHugs den Befehl
-- zeigeHistogramm langerKodierterText
-- eingeben.
langerText = (filter isMyChar.map toLower) "Eine wunderbare Heiterkeit hat meine ganze Seele eingenommen, gleich den suessen Fruehlingsmorgen, die ich mit ganzem Herzen geniesse. Ich bin allein und freue mich meines Lebens in dieser Gegend, die fuer solche Seelen geschaffen ist wie die meine. Ich bin so gluecklich, mein Bester, so ganz in dem Gefuehle von ruhigem Dasein versunken, dass meine Kunst darunter leidet. Ich koennte jetzt nicht zeichnen, nicht einen Strich, und bin nie ein groesserer Maler gewesen als in diesen Augenblicken. Wenn das liebe Tal um mich dampft, und die hohe Sonne an der Oberflaeche der undurchdringlichen Finsternis meines Waldes ruht, und nur einzelne Strahlen sich in das innere Heiligtum stehlen, ich dann im hohen Grase am fallenden Bache liege, und naeher an der Erde tausend mannigfaltige Graeschen mir merkwuerdig werden; wenn ich das Wimmeln der kleinen Welt zwischen Halmen, die unzaehligen, unergruendlichen Gestalten der Wuermchen, der Mueckchen naeher an meinem Herzen fuehle, und fuehle die Gegenwart des Allmaechtigen, der uns nach seinem Bilde schuf, das Wehen des Alliebenden, der uns in ewiger Wonne schwebend traegt und erhaelt; mein Freund! Wenn's dann um meine Augen daemmert, und die Welt um mich her und der Himmel ganz in meiner Seele ruhn wie die Gestalt einer Geliebten - dann sehne ich mich oft und denke : ach koenntest du das wieder ausdruecken, koenntest du dem Papiere das einhauchen, was so voll, so warm in dir lebt, dass es wuerde der Spiegel deiner Seele, wie deine Seele ist der Spiegel des unendlichen Gottes! - mein Freund - aber ich gehe darueber zugrunde, ich erliege unter der Gewalt der Herrlichkeit dieser Erscheinungen.Eine wunderbare Heiterkeit hat meine ganze Seele eingenommen, gleich den suessen Fruehlingsmorgen, die ich mit ganzem Herzen geniesse." where isMyChar c = isAlpha c || c == ' '
langerKodierterText = kodiere 8 langerText
