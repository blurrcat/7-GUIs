import { Elm } from './Main.elm'

var app = Elm.Main.init({
  node: document.getElementById('main')
})

function handleCircle (e, click) {
  if (e.target.nodeName == 'circle') {
    app.ports.selectedCircle.send({
      id: e.target.id,
      click: click
    })
    return true;
  } else {
    return false;
  }
}

window.addEventListener('click', (e) => {
  if (e.target.nodeName === 'svg') {
    var svg = e.target
    var point = svg.createSVGPoint()
    point.x = e.clientX
    point.y = e.clientY
    var cursor = point.matrixTransform(svg.getScreenCTM().inverse())
    app.ports.clickedPoint.send({
      x: cursor.x,
      y: cursor.y
    })
  }
  handleCircle(e, 'left')
}, false)

window.addEventListener('contextmenu', (e) => {
  if (handleCircle(e, 'right')) {
    // do not show the default menu
    e.preventDefault()
  }
}, false)
