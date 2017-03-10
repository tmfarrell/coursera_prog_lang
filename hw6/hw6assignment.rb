# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
    # The constant All_My_Pieces should be declared here
    All_My_Pieces = All_Pieces + [rotations([[0, 0], [-1, 0], [0, 1], [-1, 1], [1, 0]]), # cube + 1
                                    [[[0, 0], [-1, 0], [-2, 0], [1, 0], [2, 0]],          # extra long 
                                     [[0, 0], [0, -1], [0, -2], [0, 1], [0, 2]]],
                                    rotations([[0, 0], [0, 0], [0, 1], [1, 0]])]          # small L
    
    # your enhancements here
    def self.next_piece (board)
        MyPiece.new(All_My_Pieces.sample, board) 
    end
end

class MyBoard < Board
    # your enhancements here
    def initialize (game)
        @grid = Array.new(num_rows) {Array.new(num_columns)}
        @current_block = MyPiece.next_piece(self)
        @score = 0
        @game = game
        @delay = 500
    end
    
      def next_piece
        @current_block = MyPiece.next_piece(self)
        @current_pos = nil
      end
end

class MyTetris < Tetris
  # your enhancements here
    def initialize
        @root = TetrisRoot.new
        @timer = TetrisTimer.new
        set_board
        @running = true
        key_bindings
        my_key_bindings
        buttons
        run_game
    end
    
    def set_board
        @canvas = TetrisCanvas.new
        @board = MyBoard.new(self)
        @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
        @board.draw
    end
    
    def my_key_bindings
        @root.bind('u', proc {@board.rotate_clockwise; @board.rotate_clockwise})
    end
end


