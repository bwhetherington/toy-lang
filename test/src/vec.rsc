let StringBuilder = import("std/string.rsc").StringBuilder

fn max(a, b) {
  if b > a {
    return b
  } else {
    return a
  }
}

pub class Matrix {
  init(rows, cols) {
    self.shape = [rows, cols]
    self._data = self._init_data()
  }

  data(rows) {
    // Compute longest row
    let cols = rows
      .iter()
      .map((row) => row.len())
      .fold(0, (acc, cols) => max(acc, cols))
    
    let output = new Matrix(rows.len(), cols)

    for i in new Range(0, output.rows()) {
      let row = rows[i]
      for j in new Range(0, output.cols()) {
        let cell = 0
        if j < row.len() {
          cell = row[j]
        }
        output.set(i, j, cell)
      }
    }

    return output
  }

  identity(size) {
    let output = new Matrix(size, size)
    for i in new Range(0, size) {
      output.set(i, i, 1)
    }
    return output
  }

  translation(dx, dy) {
    // return 10
    let output = Matrix.identity(3)
    output.set(0, 2, dx)
    output.set(1, 2, dy)
    return output
  }

  scale(sx, sy) {
    let output = Matrix.identity(3)
    output.set(0, 0, sx)
    output.set(1, 1, sy)
    return output
  }

  rotation(theta) {

  }

  flip() {
    let output = new Matrix(self.cols(), self.rows())
    for row in new Range(0, self.rows()) {
      for col in new Range(0, self.cols()) {
        let val = self.get(row, col)
        output.set(col, row, val)
      }
    }
    return output
  }

  _in_bounds(row, col) {
    return 0 <= row && row < self.rows() && 0 <= col && col < self.cols()
  }

  _init_data() {
    let data = []
    for _ in new Range(0, self.rows() * self.cols()) {
      data.push(0)
    }
    return data
  }

  _index(row, col) {
    return row * self.cols() + col
  }

  rows() {
    return self.shape[0]
  }

  rows_indices_iter() {
    let n_rows = self.rows()
    let n_cols = self.cols()
    return (new Range(0, n_rows))
      .map((row_index) => (new Range(0, n_cols)))
  }

  cols_indices_iter() {

  }

  cols() {
    return self.shape[1]
  }

  get(row, col) {
    if self._in_bounds(row, col) {
      let index = self._index(row, col)
      return self._data[index]
    }
  }

  get_coords(pt) {
    return self.get(pt[0], pt[1])
  }

  set(row, col, val) {
    if self._in_bounds(row, col) {
      let index = self._index(row, col)
      self._data[index] = val
    }
  }

  set_coords(pt, val) {
    self.set(pt[0], pt[1], val)
  }

  multiply(other) {
    // Compare shapes
    // AxB * BxC
    let a_rows = self.rows()
    let a_cols = self.cols()
    let b_rows = other.rows()
    let b_cols = other.cols()


    if a_cols != b_rows {
      // Incompatible shapes
      return None
    }

    let output = new Matrix(a_rows, b_cols)

    for i in new Range(0, a_rows) {
      for j in new Range(0, b_cols) {
        // Compute cell
        let this = self
        let cell = (new Range(0, a_cols))
          .map((k) => this.get(i, k) * other.get(k, j))
          .fold(0, (sum, x) => sum + x)
        output.set(i, j, cell)
      }
    }

    return output
  }

  str() {
    let sb = new StringBuilder()
    let rows = self.rows()
    let cols = self.cols()

    for row in new Range(0, rows) {
      for col in new Range(0, cols) {
        sb.push(self.get(row, col))
        sb.push(" ")
      }
      sb.push("\n")
    }

    sb.pop()

    return sb.str()
  }
}

pub fn vec2(x, y) {
  return Matrix.data([
    [x],
    [y],
  ])
}

pub fn vec3(x, y, z) {
  return Matrix.data([
    [x],
    [y],
    [z],
  ])
}