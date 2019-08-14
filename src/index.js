import { Elm } from './Main.elm'

var app = Elm.Main.init({
  node: document.getElementById('main')
})


window.addEventListener('mouseup', (e) => {
  if (e.target.nodeName === 'svg') {
    var svg = e.target
    var point = svg.createSVGPoint()
    point.x = e.clientX
    point.y = e.clientY
    var cursor = point.matrixTransform(svg.getScreenCTM().inverse())
    app.ports.onMouseDownOnSvg.send({
      x: cursor.x,
      y: cursor.y
    })
  }
}, false)
