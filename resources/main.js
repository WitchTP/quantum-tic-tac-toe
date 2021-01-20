'use strict';

window.gameState = flyd.stream();

function getLoadCommand() {
  return {
    tag: 'Load'
  };
}

function getPlaceCommand(row, column) {
  return {
    tag: 'Place',
    contents: [row, column]
  };
}

function getDebugCommand(text) {
  return {
    tag: 'Debug',
    contents: text
  };
}

function getClearCommand() {
  return {
    tag: 'Clear'
  };
}

function loadState() {
  window.external.invoke(JSON.stringify(getLoadCommand()));
}

function placePiece(row, column) {
  window.external.invoke(JSON.stringify(getPlaceCommand(row, column)));
}

function debug(object) {
  window.external.invoke(JSON.stringify(getDebugCommand(JSON.stringify(object))));
}

function clearState() {
  window.external.invoke(JSON.stringify(getClearCommand()));
}

function preload() {
}

function create() {
  var gridColor = 0x282828;
  this.cells = {
    cellAA: { rect: this.add.rectangle(), id: 'AA', x: 0, y: 0, innerShape: undefined, value: undefined, descriptor: ['A', 'A'], lineDestination: [], rowSelector: 'tripleFirst', columnSelector: 'tripleFirst', place: placePiece.bind(null, 'A', 'A') },
    cellAB: { rect: this.add.rectangle(), id: 'AB', x: 0, y: 1, innerShape: undefined, value: undefined, descriptor: ['A', 'B'], lineDestination: [], rowSelector: 'tripleFirst', columnSelector: 'tripleSecond', place: placePiece.bind(null, 'A', 'B') },
    cellAC: { rect: this.add.rectangle(), id: 'AC', x: 0, y: 2, innerShape: undefined, value: undefined, descriptor: ['A', 'C'], lineDestination: [], rowSelector: 'tripleFirst', columnSelector: 'tripleThird', place: placePiece.bind(null, 'A', 'C') },
    cellBA: { rect: this.add.rectangle(), id: 'BA', x: 1, y: 0, innerShape: undefined, value: undefined, descriptor: ['B', 'A'], lineDestination: [], rowSelector: 'tripleSecond', columnSelector: 'tripleFirst', place: placePiece.bind(null, 'B', 'A') },
    cellBB: { rect: this.add.rectangle(), id: 'BB', x: 1, y: 1, innerShape: undefined, value: undefined, descriptor: ['B', 'B'], lineDestination: [], rowSelector: 'tripleSecond', columnSelector: 'tripleSecond', place: placePiece.bind(null, 'B', 'B') },
    cellBC: { rect: this.add.rectangle(), id: 'BC', x: 1, y: 2, innerShape: undefined, value: undefined, descriptor: ['B', 'C'], lineDestination: [], rowSelector: 'tripleSecond', columnSelector: 'tripleThird', place: placePiece.bind(null, 'B', 'C') },
    cellCA: { rect: this.add.rectangle(), id: 'CA', x: 2, y: 0, innerShape: undefined, value: undefined, descriptor: ['C', 'A'], lineDestination: [], rowSelector: 'tripleThird', columnSelector: 'tripleFirst', place: placePiece.bind(null, 'C', 'A') },
    cellCB: { rect: this.add.rectangle(), id: 'CB', x: 2, y: 1, innerShape: undefined, value: undefined, descriptor: ['C', 'B'], lineDestination: [], rowSelector: 'tripleThird', columnSelector: 'tripleSecond', place: placePiece.bind(null, 'C', 'B') },
    cellCC: { rect: this.add.rectangle(), id: 'CC', x: 2, y: 2, innerShape: undefined, value: undefined, descriptor: ['C', 'C'], lineDestination: [], rowSelector: 'tripleThird', columnSelector: 'tripleThird', place: placePiece.bind(null, 'C', 'C') }
  };
  drawGrid(this.cells, this.scale.width, this.scale.height);
  getObjectValues(this.cells).forEach(function(cell) {
    cell.rect.setFillStyle(gridColor, 1);
    cell.rect.setOrigin(0, 0);
    cell.rect.setInteractive();
    cell.rect.on('pointerdown', cell.place);
  });
  flyd.on(updateCellValuesFromBoard.bind(this, this.cells), window.gameState);
  this.scale.on('resize', resize, this);
  var clearKey = this.input.keyboard.addKey(Phaser.Input.Keyboard.KeyCodes.C);
  clearKey.on('down', clearState.bind(this));
}

function resize(gameSize) {
  var width = gameSize.width;
  var height = gameSize.height;
  this.cameras.resize(width, height);
  drawGrid(this.cells, width, height);
}

function getObjectValues(object) {
  return Object.keys(object).map(function(key) {
    return object[key];
  });
}

function drawGrid(cells, width, height) {
  var thinness = 100;
  var separationX = width / thinness;
  var separationY = height / thinness;
  getObjectValues(cells).forEach(resizeCell.bind(null, width, height, separationX, separationY));
}

function updateCellValuesFromBoard(cells, gameState) {
  getObjectValues(cells).forEach((function(cell) {
    cell.lineDestination.map(pluck('line')).forEach(function(line) {
      line.destroy();
    });
    cell.lineDestination = [];
    cell.value = gameState.gameBoard[cell.rowSelector][cell.columnSelector];
    if (cell.value === 'X' && !cell.innerShape) {
      cell.innerShape = this.add.star();
      cell.innerShape.setPoints(4);
      cell.innerShape.setOrigin(0, 0);
      cell.innerShape.setFillStyle(0x689d6a, 1);
      cell.innerShape.setInnerRadius(10);
      cell.innerShape.setOuterRadius(64);
    } else if (cell.value === 'O' && !cell.innerShape) {
      cell.innerShape = this.add.ellipse();
      cell.innerShape.setFillStyle(0xd3869b, 1);
      cell.innerShape.setOrigin(0, 0);
    } else if (cell.value === 'N' && cell.innerShape) {
      cell.innerShape.destroy();
      cell.innerShape = undefined;
    }
  }).bind(this));
  gameState.currentScoringLines.forEach((function(line) {
    var startCell = getCellByDescriptor(cells, line.lineStart);
    var endCell = getCellByDescriptor(cells, line.lineEnd);
    if (startCell.lineDestination.map(pluck('id')).indexOf(endCell.id) === -1) {
      var lineObject = this.add.line();
      lineObject.setStrokeStyle(1, 0xebdbb2, 1);
      lineObject.setOrigin(0, 0);
      lineObject.setLineWidth(10);
      startCell.lineDestination.push({ destination: endCell, line: lineObject });
    }
  }).bind(this));
  drawGrid(cells, this.scale.width, this.scale.height);
}

function concat(a, b) {
  return a.concat(b);
}

function pluck(property) {
  return function(obj) {
    return obj[property];
  }
}

function getCellByDescriptor(cells, descriptor) {
  return getObjectValues(cells).filter(function(cell) {
    return cell.descriptor[0] === descriptor[0] && cell.descriptor[1] === descriptor[1];
  })[0];
}

function resizeCell(width, height, separationX, separationY, cell) {
  cell.rect.setPosition(cell.x * width / 3 + separationX / 2, cell.y * height / 3 + separationY / 2);
  cell.rect.setSize(width / 3 - separationX, height / 3 - separationY);
  if (cell.innerShape && cell.value === 'X') {
    resizeX(cell);
  }
  if (cell.innerShape && cell.value === 'O') {
    resizeO(cell);
  }
  resizeLines(cell);
}

function resizeLines(cell) {
  cell.lineDestination.forEach(function(lineDestination) {
    lineDestination.line.setTo(cell.rect.x + cell.rect.displayWidth / 2, cell.rect.y + cell.rect.displayHeight / 2, lineDestination.destination.rect.x + lineDestination.destination.rect.displayWidth / 2, lineDestination.destination.rect.y + lineDestination.destination.rect.displayHeight / 2);
  });
}

function resizeX(cell) {
  cell.innerShape.setInnerRadius(cell.rect.displayWidth / 20);
  cell.innerShape.setOuterRadius(cell.rect.displayWidth / 2);
  cell.innerShape.setSize(cell.rect.displayWidth / 2, cell.rect.displayHeight / 2);
  cell.innerShape.setPosition(cell.rect.x, cell.rect.y);
}

function resizeO(cell) {
  cell.innerShape.setSize(cell.rect.displayWidth / 2, cell.rect.displayHeight / 2);
  cell.innerShape.setPosition(cell.rect.x + cell.rect.displayWidth / 2 - cell.innerShape.displayWidth / 2, cell.rect.y + cell.rect.displayHeight / 2 - cell.innerShape.displayHeight / 2);
}

loadState();

var config = {
  type: Phaser.AUTO,
  backgroundColor: '#ebdbb2',
  scale: {
    parent: 'phaser-game',
    mode: Phaser.Scale.FIT,
    autoCenter: Phaser.Scale.CENTER_BOTH,
    width: 1200,
    height: 1200
  },
  scene: {
    preload: preload,
    create: create
  }
};

var game = new Phaser.Game(config);
