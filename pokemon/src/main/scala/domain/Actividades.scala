package domain

import scala.util.Try
import scala.util.Success
import scala.util.Failure
  
trait Actividad extends (Estado => Estado) //sugar Function[Pokemon, Pokemon]

case object Descansar extends Actividad {
    override def apply(estado: Estado): Estado = estado.map(_.energiaAlMaximo()) match {
        case Normal(pokemon) => estado.flatmap(p => Dormido(p))
        case _ => estado
    }    
}

case class Nadar(minutos: Int) extends Actividad {
    override def apply(estado: Estado): Estado = estado.pokemon match {
        case pokemon if pokemon.esDeTipo(TipoFuego) => estado.flatmap(p => KO(p))
        case pokemon if pokemon.esDeTipo(TipoAgua) => estado.map(p => efectoBase(p).ganarVelocidad(10 * minutos / 60))
        case _ => estado.map(p => efectoBase(p))
    }

    private def efectoBase(pokemon: Pokemon): Pokemon = pokemon.perderEnergia(minutos).ganarXP(20 * minutos).ganarXP(20 * minutos)
}

case class LevantarPesas(peso: Int) extends Actividad {
    override def apply(estado:Estado): Estado = estado match {
        case Paralizado(pokemon) => estado.flatmap(p => KO(p))
        case Estado(pokemon) => pokemon match {
            case p if p.esDeTipo(TipoFantasma) => estado.flatmap(p => KO(p))
            case p if (peso > 10 * p.fuerza) => estado.flatmap(p=> Paralizado(p.perderEnergia(10)))
            case p if p.esDeTipoPrincipal(TipoPelea) => estado.flatmap(p => Normal(p.ganarXP(peso * 2)))
            case p => estado.flatmap(p =>Normal(p.ganarXP(peso)))
        }
    }
}

case class Rutina (nombre: String, actividades: List[Actividad])

object AnalizadorRutinas {
    def hacerRutina(rutina: Rutina)(estado: Estado): Estado = rutina.actividades.foldRight(estado)((actividad, estado) => estado.realizaActividad(actividad))
    def mejorRutinaSegun[O : Ordering](rutinas: List[Rutina], condicion: Estado => O, estado: Estado): Option[Rutina] = 
        rutinas.maxByOption(rutina => condicion(hacerRutina(rutina)(estado)))
}

object asd {
    val a: Estado => Estado = AnalizadorRutinas.hacerRutina(Rutina("asd", List(Descansar))) 
}

