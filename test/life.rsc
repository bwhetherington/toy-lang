let Range = iter.Range;

fn create_empty_list(length, default) {
  let list = [];
  for _ in Range.new(0, length) {
    list.push(default);
  }
  return list;
}

fn next_state(cell, count) {
  if count < 2 {
    return False;
  } else if count > 3 {
    return False;
  } else if count == 3 {
    return True;
  } else if cell {
    return True;
  } else {
    return False;
  }
}

class GameOfLife {
  init(rows, cols) {
    self._rows = rows;
    self._cols = cols;
    self._data = create_empty_list(rows * cols, False);
    self._next = create_empty_list(rows * cols, False);
  }

  _get_index(row, col) {
    row %= self._rows;
    col %= self._cols;
    return row * self._cols + col;
  }

  set_cell(row, col, val) {
    let index = self._get_index(row, col);
    self._data[index] = val;
  }

  get_cell(row, col) {
    let index = self._get_index(row, col);
    return self._data[index];
  }

  print() {
    for row in Range.new(0, self._rows) {
      for col in Range.new(0, self._cols) {
        let cell = self.get_cell(row, col);
        if cell {
          print("O ");
        } else {
          print("  ");
        }
      }
      println();
    }
  }

  count_neighbors(row, col) {
    let count = 0;
    for n_row in Range.new(row - 1, row + 2) {
      for n_col in Range.new(col - 1, col + 2) {
        n_row %= self._rows;
        n_col %= self._cols;
        if !((n_row == row) && (n_col == col)) && self.get_cell(n_row, n_col) {
          count += 1;
        }
      }
    }
    return count;
  }

  advance() {
    // Compute next board state
    for row in Range.new(0, self._rows) {
      for col in Range.new(0, self._cols) {
        let index = self._get_index(row, col);
        let neighbors = self.count_neighbors(row, col);
        let next = next_state(self._data[index], neighbors);
        self._next[index] = next;
      }
    }

    // Swap arrays
    let tmp = self._data;
    self._data = self._next;
    self._next = tmp;
  }
}

fn main() {
  // Create board
  let rows = 10;
  let cols = 10;
  let board = GameOfLife.new(rows, cols);

  // Initialize board state
  board.set_cell(0, 1, True);
  board.set_cell(1, 2, True);
  board.set_cell(2, 0, True);
  board.set_cell(2, 1, True);
  board.set_cell(2, 2, True);

  // Simulate board
  for _ in Range.new(0, 30) {
    board.print();
    board.advance();
    for _ in Range.new(0, cols) {
      print("--");
    }
    println();
  }
}

main();