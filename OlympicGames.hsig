--Importação para o funcionamento do método "sort".
import Data.List

--Atletas participantes:
listaNomeAtletas = ["Fernanda", "Milena"]

--Lançamentos realizados para o esporte "Arremesso":
listaNotaAtleta1Arremesso = [4, 5, 8]
listaNotaAtleta2Arremesso = [7, 3, 10]

--Notas realizadas para o esporte "Ginástica Artística":
listaNotaAtleta1GinasticaArtistica = [4, 5, 8, 3, 1]
listaNotaAtleta2GinasticaArtistica = [7, 3, 10, 1, 5]

--Golpes realizados para o esporte "Judô": (posição 0 = Ippon / posição 1 = Wazari / posição 2 = Yukô)
listaNotaAtleta1Judo = [0, 1, 2]
listaNotaAtleta2Judo = [0, 0, 2]

--Levantamentos realizados para o esporte "Levantamento de Peso":
listaNotaAtleta1Levantamento = 5
listaNotaAtleta2Levantamento = 10

levantamento :: [String] -> Float -> Float -> String
levantamento listaNomeAtletas x y
  | x > y =  "" ++ head (listaNomeAtletas) ++ " venceu!" --Se o primeiro participante realizar um valor de levantamento maior, ele é o vencedor.
  | y > x = "" ++ last (listaNomeAtletas) ++ " venceu!" --Se o segundo participante realizar um valor de levantamento maior, ele é o vencedor.
  | otherwise = "" ++ ("Empatou!") --Empate com os dois valores.

arremesso :: [String] -> [Int] -> [Int] -> String
arremesso listaNomeAtletas listaNotaAtleta1 listaNotaAtleta2
  | maximum listaNotaAtleta1 > maximum listaNotaAtleta2 = "" ++ head (listaNomeAtletas) ++ " venceu!" --Se o primeiro participante realizar um arremesso maior que o segundo, ele é o vencedor.
  | maximum listaNotaAtleta2 > maximum listaNotaAtleta1 = "" ++ last (listaNomeAtletas) ++ " venceu!"--Se o segundo participante realizar um arremesso maior que o primeiro, ele é o vencedor.
  | head (tail(sort listaNotaAtleta1)) > head (tail(sort listaNotaAtleta2)) = "" ++ head (listaNomeAtletas) ++ " venceu!" --Se o segundo maior arremesso do participante 1 for maior que o do participante 2, ele é o vencedor.
  | head (tail(sort listaNotaAtleta2)) > head (tail(sort listaNotaAtleta1)) = "" ++ last (listaNomeAtletas) ++ " venceu!" --Se o segundo maior arremesso do participante 2 for maior que o do participante 1, ele é o vencedor.
  | head (tail(sort listaNotaAtleta1)) == head (tail(sort listaNotaAtleta2)) = "Empatou!" --Empate com o segundo maior valor.

judo :: [String] -> [Int] -> [Int] -> String
judo listaNomeAtletas listaNotaAtleta1 listaNotaAtleta2
 | head (listaNotaAtleta1) == 1 = "" ++ head (listaNomeAtletas) ++ " venceu!" --Se o primeiro participante realizou um Ippon, ele é o vencedor.
 | head (listaNotaAtleta2) == 1 = "" ++ last (listaNomeAtletas) ++ " venceu!" --Se o segundo participante realizou um Ippon, ele é o vencedor.
 | head (tail(listaNotaAtleta1)) > head (tail(listaNotaAtleta2)) = "" ++ head (listaNomeAtletas) ++ " venceu!" --Se o primeiro participante realizou mais Wazari's, ele é o vencedor.
 | head (tail(listaNotaAtleta2)) > head (tail(listaNotaAtleta1)) = "" ++ last(listaNomeAtletas) ++ " venceu!" --Se o segundo participante realizou mais Wazari's, ele é o vencedor.
 | last(listaNotaAtleta1) > last (listaNotaAtleta2) = "" ++ head(listaNomeAtletas) ++ " venceu!" --Se o primeiro participante realizou mais Yukô's, ele é o vencedor.
 | last(listaNotaAtleta2) > last (listaNotaAtleta1) = "" ++ last(listaNomeAtletas) ++ " venceu!" --Se o segundo participante realizou mais Yukô's, ele é o vencedor.
 | last(listaNotaAtleta2) == last (listaNotaAtleta1) = " Empatou!" -- Empate com as mesmas quantidades de golpe.

ginastica :: [String] -> [Int] -> [Int] -> String
ginastica listaNomeAtletas listaNotaAtleta1 listaNotaAtleta2
  | sum (tail (sort listaNotaAtleta1)) > sum (tail (sort listaNotaAtleta2)) = "" ++ head (listaNomeAtletas) ++ " venceu!" -- Se o somatório de notas excluindo a menor do participante 1 for maior que o participante 2, ele é o vencedor.
  | sum (tail (sort listaNotaAtleta2)) > sum (tail (sort listaNotaAtleta1)) = "" ++ (last (listaNomeAtletas)) ++ " venceu!" -- Se o somatório de notas excluindo a menor do participante 2 for maior que o participante 1, ele é o vencedor.
  | sum (tail (sort listaNotaAtleta1)) == sum (tail (sort listaNotaAtleta1)) = "Empatou!" -- Empate com o mesmo somatório de notas.

main = do
  --Método do arremesso:
  print(arremesso listaNomeAtletas listaNotaAtleta1Arremesso listaNotaAtleta2Arremesso)
  --Método da ginástica:
  print(ginastica listaNomeAtletas listaNotaAtleta1GinasticaArtistica listaNotaAtleta2GinasticaArtistica)
  --Método do judô:
  print(judo listaNomeAtletas listaNotaAtleta1Judo listaNotaAtleta2Judo)
  --Método do levantamento:
  print(levantamento listaNomeAtletas listaNotaAtleta1Levantamento listaNotaAtleta2Levantamento)