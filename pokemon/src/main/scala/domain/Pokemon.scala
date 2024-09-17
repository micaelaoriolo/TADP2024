package domain

import scala.compiletime.ops.int
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.math

case class Pokemon(statsBase: Stats, especie: Especie, energia: Int, xp: Int = 0) {
    lazy val nivel = Stream.from(1).find(nivel => especie.xpNecesariaNivel(nivel) > xp) .get - 1
    
    lazy val statsActuales: Stats = especie.statsActuales(statsBase, nivel)
    lazy val fuerza: Int = statsActuales.fuerza
    lazy val velocidad: Int = statsActuales.velocidad
    lazy val energiaMaxima: Int = statsActuales.energíaMaxima

    def intentarEvolucionar(): Pokemon = especie.intentarEvolucionar(this)
    
    def esDeTipo(tipo: Tipo): Boolean = especie.esDeTipo(tipo)

    def esDeTipoPrincipal(tipo: Tipo): Boolean = especie.esDeTipoPrincipal(tipo)

    def ganarVelocidad(cantidad: Int): Pokemon = copy(statsBase = statsBase.ganarVelocidad(cantidad))
    
    def ganarXP(cantidad: Int): Pokemon = copy(xp = xp + cantidad)

    def perderEnergia(cantidad: Int): Pokemon = copy(energia = energia - cantidad)

    def energiaAlMaximo(): Pokemon = copy(energia = statsBase.energíaMaxima)
}

case class Stats(fuerza: Int, velocidad: Int, energíaMaxima: Int) {
    def ganarVelocidad(cantidad: Int): Stats = copy(velocidad = velocidad + cantidad)

    def *(m: Stats) = copy(fuerza * m.fuerza, velocidad* m.velocidad, energíaMaxima * m.energíaMaxima)

    def *(m: Int) = copy(fuerza * m, velocidad* m, energíaMaxima * m)
}

case class Especie(tipoPrincipal: Tipo, 
                    tipoSecundario: Option[Tipo], 
                    resistenciaEvolutiva: Int, 
                    multiplicadores: Stats, 
                    evolucion: Option[Evolución]) {
    def statsActuales(statsBase: Stats, nivel: Int): Stats = statsBase * multiplicadores * nivel

    def esDeTipo(tipo: Tipo): Boolean = tipoPrincipal == tipo || tipoSecundario.contains(tipo)

    def esDeTipoPrincipal(tipo: Tipo): Boolean = tipoPrincipal == tipo

    def xpNecesariaNivel(nivel: Int): Int = (scala.math.pow(2, nivel).toInt - 1) * resistenciaEvolutiva

    def intentarEvolucionar(pokemon: Pokemon): Pokemon = evolucion.flatMap(_.evolucionar(pokemon)).getOrElse(pokemon)
}

case class Evolución(esp: Especie, condicion: Pokemon => Boolean) {
    def evolucionar(pokemon: Pokemon): Option[Pokemon] = if(condicion(pokemon)) Some(pokemon.copy(especie = esp)) else None
}

trait Tipo

case object TipoAgua extends Tipo

case object TipoFantasma extends Tipo

case object TipoPelea extends Tipo

case object TipoFuego extends Tipo



