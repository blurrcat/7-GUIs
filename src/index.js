import { Elm as Counter } from './Counter.elm'
import { Elm as FlightBooker } from './FlightBooker.elm'
import { Elm as Timer } from './Timer.elm'
import { Elm as TemperatureConverter } from './TemperatureConverter'

FlightBooker.FlightBooker.init({
  node: document.getElementById("flight-booker")
})

Timer.Timer.init({
  node: document.getElementById("timer")
})

Counter.Counter.init({
  node: document.getElementById("counter")
})

TemperatureConverter.TemperatureConverter.init({
  node: document.getElementById("temperature")
})
