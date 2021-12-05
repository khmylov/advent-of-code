const inputText = `
77, 279
216, 187
72, 301
183, 82
57, 170
46, 335
55, 89
71, 114
313, 358
82, 88
78, 136
339, 314
156, 281
260, 288
125, 249
150, 130
210, 271
190, 258
73, 287
187, 332
283, 353
66, 158
108, 97
237, 278
243, 160
61, 52
353, 107
260, 184
234, 321
181, 270
104, 84
290, 109
193, 342
43, 294
134, 211
50, 129
92, 112
309, 130
291, 170
89, 204
186, 177
286, 302
188, 145
40, 52
254, 292
270, 287
238, 216
299, 184
141, 264
117, 129
`

function max(xs, f) {
  const seed = f(xs[0]);
  return xs.reduce((acc, x) => {
    const fx = f(x);
    return Math.max(fx, acc);
  }, seed);
}

let input = inputText.split(/\r|\n/).filter(x => x.length).map((x, i) => {
  const split = x.split(', ');
  return { Id: i, X: parseInt(split[0]), Y: parseInt(split[1]) };
});

const maxX = max(input, x => x.X);
const maxY = max(input, x => x.Y);

const DRAW_SCALE = 3;

const chart = document.getElementById('chart');
chart.width = (maxX + 1) * DRAW_SCALE;
chart.height = (maxY + 1) * DRAW_SCALE;
const chartCtx = chart.getContext('2d');

function* buildPath(fromX, fromY, toX, toY) {
  const visited = {};
  const visit = (x, y) => visited[`${x};${y}`] = true;
  const canVisit = (x, y) => x >= 0 && y >= 0 && x <= toX && y <= toY && !visited[`${x};${y}`];

  const queue = [];
  queue.push([fromX, fromY]);

  while (queue.length) {
    const [x, y] = queue.shift();
    if (canVisit(x, y)) {
      visit(x, y);
      yield [x, y];
      queue.push([x - 1, y]);
      queue.push([x + 1, y]);
      queue.push([x, y + 1]);
      queue.push([x, y - 1]);
    }
  }
}

function getDistance(fromX, fromY, toX, toY) {
  return Math.abs(fromX - toX) + Math.abs(fromY - toY);
}

function isCoord(x, y) {
  return Boolean(input.find(c => c.X === x && c.Y === y));
}

const colorMap = ["#FFFF00", "#1CE6FF", "#FF34FF", "#FF4A46", "#008941", "#006FA6", "#A30059",
  "#FFDBE5", "#7A4900", "#0000A6", "#63FFAC", "#B79762", "#004D43", "#8FB0FF", "#997D87",
  "#5A0007", "#809693", "#FEFFE6", "#1B4400", "#4FC601", "#3B5DFF", "#4A3B53", "#FF2F80",
  "#61615A", "#BA0900", "#6B7900", "#00C2A0", "#FFAA92", "#FF90C9", "#B903AA", "#D16100",
  "#DDEFFF", "#000035", "#7B4F4B", "#A1C299", "#300018", "#0AA6D8", "#013349", "#00846F",
  "#372101", "#FFB500", "#C2FFED", "#A079BF", "#CC0744", "#C0B9B2", "#C2FF99", "#001E09",
  "#00489C", "#6F0062", "#0CBD66", "#EEC3FF", "#456D75", "#B77B68", "#7A87A1", "#788D66",
  "#885578", "#FAD09F", "#FF8A9A", "#D157A0", "#BEC459", "#456648", "#0086ED", "#886F4C",
  "#34362D", "#B4A8BD", "#00A6AA", "#452C2C", "#636375", "#A3C8C9", "#FF913F", "#938A81",
  "#575329", "#00FECF", "#B05B6F", "#8CD0FF", "#3B9700", "#04F757", "#C8A1A1", "#1E6E00",
  "#7900D7", "#A77500", "#6367A9", "#A05837", "#6B002C", "#772600", "#D790FF", "#9B9700",
  "#549E79", "#FFF69F", "#201625", "#72418F", "#BC23FF", "#99ADC0", "#3A2465", "#922329",
  "#5B4534", "#FDE8DC", "#404E55", "#0089A3", "#CB7E98", "#A4E804", "#324E72", "#6A3A4C"]

function rectAt(x, y) {
  chartCtx.fillRect(x * DRAW_SCALE, y * DRAW_SCALE, DRAW_SCALE, DRAW_SCALE);
}

function setPoint(x, y, coordId) {
  if (isCoord(x, y)) {
    return;
  }
  chartCtx.fillStyle = colorMap[coordId % colorMap.length];
  rectAt(x, y);
}

function setMultiplePoint(x, y) {
  if (isCoord(x, y)) {
    return;
  }
  chartCtx.fillStyle = 'cyan';
  rectAt(x, y);
}

function setCoordCenter(x, y) {
  chartCtx.fillStyle = 'black';
  rectAt(x, y);
}

function delay(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

async function buildGrid(coords) {
  const grid = Array.from({ length: maxY + 1 }).map(() => Array.from({ length: maxX + 1 }).map(() => ({ kind: 'empty' })));
  let counter = 0;

  async function tryDelay() {
    counter++;
    if (counter % 120 === 0) {
      await delay(1);
    }
  }

  for (const coord of coords) {
    for (const [x, y] of buildPath(coord.X, coord.Y, maxX, maxY)) {
      const distance = getDistance(coord.X, coord.Y, x, y);

      const existingValue = grid[y][x];
      if (existingValue.kind === 'empty') {
        grid[y][x] = { kind: 'one', distance };
        setPoint(x, y, coord.Id);
        await tryDelay();
      } else if (existingValue.kind === 'one') {
        if (distance < existingValue.distance) {
          grid[y][x] = { kind: 'one', distance };
          setPoint(x, y, coord.Id);
          await tryDelay();
        } else if (distance === existingValue.distance) {
          grid[y][x] = { kind: 'multiple', distance };
          setMultiplePoint(x, y);
          await tryDelay();
        }
      } else if (existingValue.kind === 'multiple' && distance < existingValue.distance) {
        grid[y][x] = { kind: 'one', distance };
        setPoint(x, y, coord.Id);
        await tryDelay();
      }
    }
  }
}

async function run() {
  for (const coord of input) {
    setCoordCenter(coord.X, coord.Y);
  }

  await buildGrid(input);
  console.log('Done');
}

run();
