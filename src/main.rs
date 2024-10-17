use std::thread::current;

use macroquad::prelude::*;

pub fn get_conf() -> Conf {
    Conf {
        window_title: "Rust Chess Engine".to_string(),
        window_width: 1,
        window_height: 1,
        fullscreen: true,
        ..Default::default()
    }
}

pub struct Board {
    pub dark_tile_color: Color,
    pub light_tile_color: Color,
    pub cell_width: f32,
    pub cell_height: f32,
    pub vertical_cell_count: u32,
    pub horizontal_cell_count: u32,
    pub pieces: Vec<Option<Piece>>,
    pub white_king_side_castle_possible: bool,
    pub white_queen_side_castle_possible: bool,
    pub black_king_side_castle_possible: bool,
    pub black_queen_side_castle_possible: bool,
    pub events: Vec<Event>,
    pub current_player: Faction,
    pub game_state: GameState
}

impl Board {
    pub fn new(dark_tile_color: Color, light_tile_color: Color, cell_width: f32, cell_height: f32, vertical_cell_count: u32, horizontal_cell_count: u32) -> Self {
        Board {
            dark_tile_color, 
            light_tile_color, 
            cell_width, cell_height, 
            vertical_cell_count, 
            horizontal_cell_count, 
            pieces: vec![None; vertical_cell_count as usize * horizontal_cell_count as usize], 
            white_king_side_castle_possible: true, 
            white_queen_side_castle_possible: true, 
            black_king_side_castle_possible: true, 
            black_queen_side_castle_possible: true,
            events: Vec::new(),
            current_player: Faction::White,
            game_state: GameState::Nominal
        }
    }

    pub fn block_white_castle(&mut self) {
        self.block_white_king_side_castle();
        self.block_white_queen_side_castle();
    }

    pub fn block_black_castle(&mut self) {
        self.block_black_king_side_castle();
        self.block_black_queen_side_castle();
    }

    pub fn block_white_king_side_castle(&mut self) {
        if self.white_king_side_castle_possible {
            self.white_king_side_castle_possible = false;
            self.events.push(Event::BlockWhiteKingSideCastle);
        }
    }

    pub fn block_white_queen_side_castle(&mut self) {
        if self.white_queen_side_castle_possible {
            self.white_queen_side_castle_possible = false;
            self.events.push(Event::BlockWhiteQueenSideCastle);
        }
    }

    pub fn block_black_king_side_castle(&mut self) {
        if self.black_king_side_castle_possible {
            self.black_king_side_castle_possible = false;
            self.events.push(Event::BlockBlackKingSideCastle);
        }
    }

    pub fn block_black_queen_side_castle(&mut self) {
        if self.black_queen_side_castle_possible {
            self.black_queen_side_castle_possible = false;
            self.events.push(Event::BlockBlackQueenSideCastle);
        }
    }

    pub fn add_piece(&mut self, position: UVec2, piece: Piece) {
        let index = self.get_piece_index(position);
        if let Some(slot) = self.pieces.get_mut(index) {
            *slot = Some(piece);
        }
    }

    pub fn generate_standard_piece_set(&mut self) {
        self.add_piece(UVec2::new(4, 7), Piece::WhiteKing);
        self.add_piece(UVec2::new(3, 7), Piece::WhiteQueen);
        self.add_piece(UVec2::new(2, 7), Piece::WhiteBishop);
        self.add_piece(UVec2::new(5, 7), Piece::WhiteBishop);
        self.add_piece(UVec2::new(1, 7), Piece::WhiteKnight);
        self.add_piece(UVec2::new(6, 7), Piece::WhiteKnight);
        self.add_piece(UVec2::new(0, 7), Piece::WhiteRook);
        self.add_piece(UVec2::new(7, 7), Piece::WhiteRook);
        self.add_piece(UVec2::new(0, 6), Piece::WhitePawn);
        self.add_piece(UVec2::new(1, 6), Piece::WhitePawn);
        self.add_piece(UVec2::new(2, 6), Piece::WhitePawn);
        self.add_piece(UVec2::new(3, 6), Piece::WhitePawn);
        self.add_piece(UVec2::new(4, 6), Piece::WhitePawn);
        self.add_piece(UVec2::new(5, 6), Piece::WhitePawn);
        self.add_piece(UVec2::new(6, 6), Piece::WhitePawn);
        self.add_piece(UVec2::new(7, 6), Piece::WhitePawn);

        self.add_piece(UVec2::new(4, 0), Piece::BlackKing);
        self.add_piece(UVec2::new(3, 0), Piece::BlackQueen);
        self.add_piece(UVec2::new(2, 0), Piece::BlackBishop);
        self.add_piece(UVec2::new(5, 0), Piece::BlackBishop);
        self.add_piece(UVec2::new(1, 0), Piece::BlackKnight);
        self.add_piece(UVec2::new(6, 0), Piece::BlackKnight);
        self.add_piece(UVec2::new(0, 0), Piece::BlackRook);
        self.add_piece(UVec2::new(7, 0), Piece::BlackRook);
        self.add_piece(UVec2::new(0, 1), Piece::BlackPawn);
        self.add_piece(UVec2::new(1, 1), Piece::BlackPawn);
        self.add_piece(UVec2::new(2, 1), Piece::BlackPawn);
        self.add_piece(UVec2::new(3, 1), Piece::BlackPawn);
        self.add_piece(UVec2::new(4, 1), Piece::BlackPawn);
        self.add_piece(UVec2::new(5, 1), Piece::BlackPawn);
        self.add_piece(UVec2::new(6, 1), Piece::BlackPawn);
        self.add_piece(UVec2::new(7, 1), Piece::BlackPawn);
    }

    pub fn draw_board(&self) {
        let total_width = self.vertical_cell_count as f32 * self.cell_width;
        let total_height = self.horizontal_cell_count as f32 * self.cell_height;
        let mut dark_tile = false;

        for y in 0..self.horizontal_cell_count {
            for x in 0..self.vertical_cell_count {
                let color = match dark_tile {
                    true => self.dark_tile_color,
                    false => self.light_tile_color
                };
                draw_rectangle(x as f32 * self.cell_width + screen_width() / 2.0 - total_width / 2.0, y as f32 * self.cell_height + screen_height() / 2.0 - total_height / 2.0, self.cell_width, self.cell_height, color);
                dark_tile = !dark_tile;
            }

            if self.horizontal_cell_count % 2 == 0 {
                dark_tile = !dark_tile;
            }
        }
    }

    pub fn draw_pieces(&self, textures: &PieceTextures, piece_size: Vec2, maybe_ghost_position: Option<UVec2>) {
        for y in 0..self.horizontal_cell_count {
            for x in 0..self.vertical_cell_count {
                let position = UVec2::new(x, y);
                let color = match maybe_ghost_position {
                    Some(ghost_position) if ghost_position == position => Color::from_rgba(255, 255, 255, 100),
                    _ => WHITE
                };
                if let Some(Some(piece)) = self.pieces.get(self.get_piece_index(position)) {
                    render_piece(*piece, textures, self, UVec2::new(x, y), piece_size, color);
                }
            }
        }
    }

    pub fn get_tile_position(&self, position: UVec2) -> Vec2 {
        let total_size = Vec2::new(self.cell_width * self.horizontal_cell_count as f32, self.cell_height * self.vertical_cell_count as f32);
        Vec2::new(position.x as f32 * self.cell_width + self.cell_width / 2.0, position.y as f32 * self.cell_height + self.cell_height / 2.0) + Vec2::new(screen_width(), screen_height()) / 2.0 - total_size / 2.0
    }

    pub fn get_piece_index(&self, position: UVec2) -> usize {
        (position.x + position.y * self.horizontal_cell_count) as usize
    }

    pub fn get_piece_slot_mut(&mut self, position: UVec2) -> Option<&mut Option<Piece>> {
        let index = self.get_piece_index(position);
        self.pieces.get_mut(index)
    }

    pub fn get_piece(&self, position: UVec2) -> Option<Piece> {
        self.pieces.get(self.get_piece_index(position)).map(|x| x.clone()).flatten()
    }

    pub fn get_greatest_index(&self) -> usize {
        self.horizontal_cell_count as usize * self.vertical_cell_count as usize - 1
    }

    pub fn move_piece(&mut self, from: UVec2, to: UVec2) -> Option<Piece> {
        let Some(piece) = self.get_piece(from) else {
            return None;
        };

        let displaced_piece = self.get_piece(to);

        if let Some(from_slot) = self.get_piece_slot_mut(from) {
            *from_slot = None;
        };

        if let Some(to_slot) = self.get_piece_slot_mut(to) {
            *to_slot = Some(piece);
        };

        return displaced_piece;
    }

    pub fn undo(&mut self) {
        while let Some(event) = self.events.pop() {
            match event {
                Event::Move(Move::Move(_piece, from, to)) => {
                    self.move_piece(to, from);
                    break;
                },
                Event::Move(Move::Castle(_king, king_from, king_to, _rook, rook_from, rook_to)) => {
                    self.move_piece(king_to, king_from);
                    self.move_piece(rook_to, rook_from);
                    break;
                },
                Event::Move(Move::Capture(_, from, to, victim)) => {
                    self.move_piece(to, from);
                    self.add_piece(to, victim);
                    break;
                }
                Event::BlockWhiteKingSideCastle => self.white_king_side_castle_possible = true,
                Event::BlockWhiteQueenSideCastle => self.white_queen_side_castle_possible = true,
                Event::BlockBlackKingSideCastle => self.black_king_side_castle_possible = true,
                Event::BlockBlackQueenSideCastle => self.black_queen_side_castle_possible = true,
                Event::GameEnded => self.game_state = GameState::Nominal
            }
        }
    }

    pub fn switch_player(&mut self) {
        self.current_player = match self.current_player {
            Faction::White => Faction::Black,
            Faction::Black => Faction::White
        }
    }

    pub fn find_piece(&self, piece: Piece) -> Option<UVec2> {
        for y in 0..self.horizontal_cell_count {
            for x in 0..self.vertical_cell_count {
                let position = UVec2::new(x, y);
                if self.get_piece(position).map_or(false, |x| x == piece) {
                    return Some(position)
                }
            }
        }
        return None;
    }

    pub fn is_in_check(&self, faction: Faction) -> bool {
        let king = match faction {
            Faction::White => Piece::WhiteKing,
            Faction::Black => Piece::BlackKing
        };

        let Some(position) = self.find_piece(king) else {
            return false;
        };

        let check_moves = get_all_check_locations(self, king);

        return check_moves.contains(&position);
    }

    pub fn get_moves(position: UVec2) -> (Vec<UVec2>, Vec<(UVec2, UVec2, UVec2)>, Vec<UVec2>, Vec<UVec2>) {
        
    }
}

pub enum Directions {
    Straight,
    Diagonal,
    All,
    Knight
}

impl Directions {
    pub fn to_vecs(self) -> Vec<IVec2> {
        match self {
            Self::Straight => vec![IVec2::new(-1, 0), IVec2::new(0, -1), IVec2::new(1, 0), IVec2::new(0, 1)],
            Self::Diagonal => vec![IVec2::new(-1, -1), IVec2::new(1, -1), IVec2::new(-1, 1), IVec2::new(1, 1)],
            Self::All => vec![IVec2::new(-1, -1), IVec2::new(1, -1), IVec2::new(-1, 1), IVec2::new(1, 1), IVec2::new(-1, 0), IVec2::new(0, -1), IVec2::new(1, 0), IVec2::new(0, 1)],
            Self::Knight => vec![IVec2::new(2, 1), IVec2::new(2, -1), IVec2::new(-2, 1), IVec2::new(-2, -1), IVec2::new(1, 2), IVec2::new(1, -2), IVec2::new(-1, 2), IVec2::new(-1, -2)]
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Piece {
    WhiteKing,
    WhiteQueen,
    WhiteRook,
    WhiteBishop,
    WhiteKnight,
    WhitePawn,
    BlackKing,
    BlackQueen,
    BlackRook,
    BlackBishop,
    BlackKnight,
    BlackPawn
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Faction {
    White,
    Black
}

impl Piece {
    pub fn get_movement_destinations(&self, board: &Board, position: UVec2) -> Vec<UVec2> {
        let white_pawn_bonus = match position.y {
            6 => 2,
            _ => 1
        };
        let black_pawn_bonus = match position.y {
            1 => 2,
            _ => 1
        };

        match self {
            Piece::WhiteKing | Piece::BlackKing => generate_line_moves_sets(board, *self, position, Directions::All.to_vecs(), ObstructionHandling::Stop, 1),
            Piece::WhiteQueen | Piece::BlackQueen => generate_line_moves_sets(board, *self, position, Directions::All.to_vecs(), ObstructionHandling::Stop, 8),
            Piece::WhiteKnight | Piece::BlackKnight => generate_line_moves_sets(board, *self, position, Directions::Knight.to_vecs(), ObstructionHandling::Hop, 1),
            Piece::WhiteRook | Piece::BlackRook => generate_line_moves_sets(board, *self, position, Directions::Straight.to_vecs(), ObstructionHandling::Stop, 8),
            Piece::WhiteBishop | Piece::BlackBishop => generate_line_moves_sets(board, *self, position, Directions::Diagonal.to_vecs(), ObstructionHandling::Stop, 8),

            Piece::WhitePawn => generate_line_moves(board, *self, position, IVec2::new(0, -1), ObstructionHandling::Stop, white_pawn_bonus),
            Piece::BlackPawn => generate_line_moves(board, *self, position, IVec2::new(0, 1), ObstructionHandling::Stop, black_pawn_bonus)
        }
    }

    pub fn get_capure_destinations(&self, board: &Board, position: UVec2, capture_mode: ObstructionHandling) -> Vec<UVec2> {
        match self {
            Piece::WhiteKing | Piece::BlackKing => generate_line_moves_sets(board, *self, position, Directions::All.to_vecs(), capture_mode, 1),
            Piece::WhiteQueen | Piece::BlackQueen => generate_line_moves_sets(board, *self, position, Directions::All.to_vecs(), capture_mode, 8),
            Piece::WhiteKnight | Piece::BlackKnight => generate_line_moves_sets(board,*self,  position, Directions::Knight.to_vecs(), capture_mode, 1),
            Piece::WhiteRook | Piece::BlackRook => generate_line_moves_sets(board, *self, position, Directions::Straight.to_vecs(), capture_mode, 8),
            Piece::WhiteBishop | Piece::BlackBishop => generate_line_moves_sets(board, *self, position, Directions::Diagonal.to_vecs(), capture_mode, 8),

            Piece::WhitePawn => generate_line_moves_sets(board, *self, position, vec![IVec2::new(-1, -1), IVec2::new(1, -1)], capture_mode, 1),
            Piece::BlackPawn => generate_line_moves_sets(board, *self, position, vec![IVec2::new(-1, 1), IVec2::new(1, 1)], capture_mode, 1)
        }
    }

    pub fn get_castling_destinations(&self, board: &Board) -> Vec<(UVec2, Piece, UVec2, UVec2)> {
        let mut moves = Vec::new();
        match self {
            Piece::WhiteKing => {
                if board.get_piece(UVec2::new(5, 7)).is_none() && board.get_piece(UVec2::new(6, 7)).is_none() && board.white_king_side_castle_possible {
                    moves.push((UVec2::new(6, 7), Piece::WhiteRook, UVec2::new(7, 7), UVec2::new(5, 7)))
                }

                if board.get_piece(UVec2::new(1, 7)).is_none() && board.get_piece(UVec2::new(2, 7)).is_none() && board.get_piece(UVec2::new(3, 7)).is_none() && board.white_queen_side_castle_possible {
                    moves.push((UVec2::new(1, 7), Piece::WhiteRook, UVec2::new(0, 7), UVec2::new(2, 7)))
                }
            },
            Piece::BlackKing => {
                if board.get_piece(UVec2::new(5, 0)).is_none() && board.get_piece(UVec2::new(6, 0)).is_none() && board.black_king_side_castle_possible {
                    moves.push((UVec2::new(6, 0), Piece::BlackRook, UVec2::new(7, 0), UVec2::new(5, 0)))
                }

                if board.get_piece(UVec2::new(1, 0)).is_none() && board.get_piece(UVec2::new(2, 0)).is_none() && board.get_piece(UVec2::new(3, 0)).is_none() && board.black_queen_side_castle_possible {
                    moves.push((UVec2::new(1, 0), Piece::BlackRook, UVec2::new(0, 0), UVec2::new(2, 0)))
                }
            }
            _ => ()
        }
        return moves;
    }

    pub fn get_faction(&self) -> Faction {
        match self {
            Piece::WhiteKing => Faction::White,
            Piece::WhiteQueen => Faction::White,
            Piece::WhiteRook => Faction::White,
            Piece::WhiteBishop => Faction::White,
            Piece::WhiteKnight => Faction::White,
            Piece::WhitePawn => Faction::White,
            
            Piece::BlackKing => Faction::Black,
            Piece::BlackQueen => Faction::Black,
            Piece::BlackRook => Faction::Black,
            Piece::BlackBishop => Faction::Black,
            Piece::BlackKnight => Faction::Black,
            Piece::BlackPawn => Faction::Black,
        }
    }
}

pub fn draw_moves(board: &Board, destinations: impl Iterator<Item=UVec2>, color: Color) {
    for destination in destinations {
        let position = board.get_tile_position(destination);
        draw_rectangle(position.x - 25.0, position.y - 25.0, 50.0, 50.0, color);
    }
}

pub fn is_valid_position(board: &Board, position: IVec2) -> bool {
    position.x >= 0 && position.x < board.horizontal_cell_count as i32 && position.y >= 0 && position.y < board.vertical_cell_count as i32
}

pub fn is_position_blocked(board: &Board, position: IVec2) -> bool {
    if !is_valid_position(board, position) {
        return true;
    }
    return board.get_piece(position.as_uvec2()).is_some();
}

pub fn is_position_blocked_by_enemy(board: &Board, piece: Piece, position: IVec2) -> bool {
    if !is_valid_position(board, position) {
        return false;
    }
    if let Some(other_piece) = board.get_piece(position.as_uvec2()) {
        return other_piece.get_faction() != piece.get_faction();
    } else {
        return false;
    }
}

pub fn get_all_check_locations(board: &Board, piece: Piece) -> Vec<UVec2> {
    let mut positions = Vec::new();

    for x in 0..board.horizontal_cell_count {
        for y in 0..=board.vertical_cell_count {
            let position = UVec2::new(x, y);
            if let Some(other_piece) = board.get_piece(position) {
                if other_piece.get_faction() != piece.get_faction() {
                    positions.append(&mut other_piece.get_capure_destinations(board, position, ObstructionHandling::Check));
                }
            }
        }
    }

    return positions;
}

#[derive(PartialEq, Clone, Copy)]
pub enum ObstructionHandling {
    Ignore,
    Hop,
    Stop,
    Capture,
    Check
}

impl ObstructionHandling {
    pub fn needs_enemy_obstruction(&self) -> bool {
        match self {
            ObstructionHandling::Ignore => false,
            ObstructionHandling::Hop => false,
            ObstructionHandling::Stop => false,
            ObstructionHandling::Capture => true,
            ObstructionHandling::Check => false
        }
    }
}

pub fn generate_line_moves(board: &Board, piece: Piece, start_position: UVec2, delta_position: IVec2, blocking: ObstructionHandling, length: u32) -> Vec<UVec2> {
    let mut position = start_position.as_ivec2();
    let mut positions = Vec::new();
    for _ in 1..=length {
        position += delta_position;
        if !is_valid_position(board, position) {
            continue;
        }

        if is_position_blocked(board, position) && blocking == ObstructionHandling::Stop {
            break;
        } else if is_position_blocked(board, position) && blocking == ObstructionHandling::Hop {
            continue;
        }

        if !blocking.needs_enemy_obstruction() || is_position_blocked_by_enemy(board, piece, position) {
            positions.push(position.as_uvec2());
        }

        if is_position_blocked(board, position) && (blocking == ObstructionHandling::Capture || blocking == ObstructionHandling::Check) {
            break;
        }
    }

    return positions;
}

pub fn generate_line_moves_sets(board: &Board, piece: Piece, start_position: UVec2, delta_positions: Vec<IVec2>, blocking: ObstructionHandling, length: u32) -> Vec<UVec2> {
    let mut positions = Vec::new();

    for delta_position in delta_positions {
        positions.append(&mut generate_line_moves(board, piece, start_position, delta_position, blocking, length))
    }

    return positions;
}

pub struct PieceTextures {
    white_king_texture: Texture2D,
    white_queen_texture: Texture2D,
    white_rook_texture: Texture2D,
    white_bishop_texture: Texture2D,
    white_knight_texture: Texture2D,
    white_pawn_texture: Texture2D,

    black_king_texture: Texture2D,
    black_queen_texture: Texture2D,
    black_rook_texture: Texture2D,
    black_bishop_texture: Texture2D,
    black_knight_texture: Texture2D,
    black_pawn_texture: Texture2D,
}

impl PieceTextures {
    pub async fn new() -> Result<PieceTextures, macroquad::Error> {
        Ok(PieceTextures {
            white_king_texture: load_texture("assets/textures/pieces/white_king.png").await?,
            white_queen_texture: load_texture("assets/textures/pieces/white_queen.png").await?,
            white_rook_texture: load_texture("assets/textures/pieces/white_rook.png").await?,
            white_bishop_texture: load_texture("assets/textures/pieces/white_bishop.png").await?,
            white_knight_texture: load_texture("assets/textures/pieces/white_knight.png").await?,
            white_pawn_texture: load_texture("assets/textures/pieces/white_pawn.png").await?,

            black_king_texture: load_texture("assets/textures/pieces/black_king.png").await?,
            black_queen_texture: load_texture("assets/textures/pieces/black_queen.png").await?,
            black_rook_texture: load_texture("assets/textures/pieces/black_rook.png").await?,
            black_bishop_texture: load_texture("assets/textures/pieces/black_bishop.png").await?,
            black_knight_texture: load_texture("assets/textures/pieces/black_knight.png").await?,
            black_pawn_texture: load_texture("assets/textures/pieces/black_pawn.png").await?,
        })
    }

    pub fn get_texture(&self, piece: Piece) -> &Texture2D {
        match piece {
            Piece::WhiteKing => &self.white_king_texture,
            Piece::WhiteQueen => &self.white_queen_texture,
            Piece::WhiteRook => &self.white_rook_texture,
            Piece::WhiteBishop => &self.white_bishop_texture,
            Piece::WhiteKnight => &self.white_knight_texture,
            Piece::WhitePawn => &self.white_pawn_texture,

            Piece::BlackKing => &self.black_king_texture,
            Piece::BlackQueen => &self.black_queen_texture,
            Piece::BlackRook => &self.black_rook_texture,
            Piece::BlackBishop => &self.black_bishop_texture,
            Piece::BlackKnight => &self.black_knight_texture,
            Piece::BlackPawn => &self.black_pawn_texture,
        }
    }
}

pub fn render_piece(piece: Piece, textures: &PieceTextures, board: &Board, position: UVec2, piece_size: Vec2, color: Color) {
    let point = board.get_tile_position(position) - piece_size / 2.0;
    draw_texture_ex(textures.get_texture(piece), point.x, point.y, color, DrawTextureParams {dest_size: Some(piece_size), ..Default::default()});
}

#[derive(Clone, Copy)]
pub struct HeldPiece {
    pub position: UVec2,
    pub piece: Piece,
    pub offset: Vec2
}

impl HeldPiece {
    pub fn new(position: UVec2, offset: Vec2, piece: Piece) -> Self {
        HeldPiece {position, offset, piece}
    }
}

#[derive(Clone, Copy)]
pub enum Move {
    Move(Piece, UVec2, UVec2),
    Capture(Piece, UVec2, UVec2, Piece),
    Castle(Piece, UVec2, UVec2, Piece, UVec2, UVec2)
}

pub enum Event {
    Move(Move),
    BlockWhiteKingSideCastle,
    BlockWhiteQueenSideCastle,
    BlockBlackKingSideCastle,
    BlockBlackQueenSideCastle,
    GameEnded
}

#[derive(PartialEq)]
pub enum GameState {
    Nominal,
    WhiteCheckmated,
    BlackCheckmated,
    Statemate
}

pub enum AutoUndo {
    Idle,
    Waiting(f32)
}

impl AutoUndo {
    pub fn is_waiting(&self) -> bool {
        match self {
            AutoUndo::Idle => false,
            AutoUndo::Waiting(_) => true
        }
    }

    pub fn is_ready(&self) -> bool {
        match self {
            AutoUndo::Waiting(0.0) => true,
            _ => false
        }
    }

    pub fn wait(&mut self, time: f32) {
        *self = match self {
            AutoUndo::Idle => AutoUndo::Idle,
            AutoUndo::Waiting(wait) => AutoUndo::Waiting(f32::max(0.0, *wait - time))
        }
    }
}

#[macroquad::main(get_conf)]
pub async fn main() {
    while screen_width() == 1.0 {
        next_frame().await;
    }

    let mut board = Board::new(Color::from_hex(0x759680), Color::from_hex(0xddede2), 125.0, 125.0, 8, 8);
    let mut maybe_held_piece: Option<HeldPiece> = None;
    board.generate_standard_piece_set();
    let textures = PieceTextures::new().await.expect("Failed to load textures.");
    let auto_undo_delay: f32 = 0.1;
    let mut auto_undo: AutoUndo = AutoUndo::Idle;

    loop {
        let (mouse_x, mouse_y) = mouse_position();
        let mouse_position = Vec2::new(mouse_x, mouse_y);

        clear_background(Color::new(0.15, 0.15, 0.2, 0.1));

        let mut maybe_position: Option<UVec2> = None;
        for x in 0..board.horizontal_cell_count {
            for y in 0..board.vertical_cell_count {
                let position = board.get_tile_position(UVec2::new(x, y));
                if mouse_position.x > position.x - board.cell_width / 2.0 && mouse_position.x < position.x + board.cell_width / 2.0 && mouse_position.y > position.y - board.cell_height / 2.0 && mouse_position.y < position.y + board.cell_height / 2.0 {
                    maybe_position = Some(UVec2::new(x, y));
                    break;
                }
            }
        }

        if let (None, true, Some(position)) = (maybe_held_piece, is_mouse_button_pressed(MouseButton::Left), maybe_position) {
        if let Some(piece) = board.get_piece(position) {
                if piece.get_faction() == board.current_player {
                    maybe_held_piece = Some(HeldPiece::new(maybe_position.unwrap(), board.get_tile_position(maybe_position.unwrap()) - mouse_position, piece));
                }
            }
        }

        board.draw_board();
        board.draw_pieces(&textures, Vec2::splat(100.0), maybe_held_piece.map(|piece| piece.position));

        let maybe_drawn_moves_piece = match (maybe_held_piece, maybe_position) {
            (Some(piece), _) => Some(piece.position),
            (None, Some(position)) => Some(position),
            _ => None
        };

        let mut maybe_moves: Option<Vec<UVec2>> = None;
        let mut maybe_castling_moves: Option<Vec<(UVec2, Piece, UVec2, UVec2)>> = None;
        let mut maybe_capturing_moves: Option<Vec<UVec2>> = None;
        let mut maybe_blocked_moves: Option<Vec<UVec2>> = None;
        if let Some(drawn_moves_piece) = maybe_drawn_moves_piece {
            if let Some(piece) = board.get_piece(drawn_moves_piece) {
                if piece.get_faction() == board.current_player {
                    let mut blocked_moves = Vec::new();

                    let mut moves = piece.get_movement_destinations(&board, drawn_moves_piece);
                    moves.retain(|potential_move| {
                        board.move_piece(drawn_moves_piece, *potential_move);
                        let blocked = board.is_in_check(board.current_player);
                        board.move_piece(*potential_move, drawn_moves_piece);
                        if blocked {
                            blocked_moves.push(*potential_move);
                        }
                        return !blocked;
                    });

                    let mut capturing_moves = piece.get_capure_destinations(&board, drawn_moves_piece, ObstructionHandling::Capture);
                    capturing_moves.retain(|potential_move| {
                        let maybe_defeated = board.move_piece(drawn_moves_piece, *potential_move);
                        let blocked = board.is_in_check(board.current_player);
                        board.move_piece(*potential_move, drawn_moves_piece);
                        if let Some(defeated) = maybe_defeated {
                            board.add_piece(*potential_move, defeated);
                        }
                        if blocked {
                            blocked_moves.push(*potential_move);
                        }
                        return !blocked;
                    });

                    let mut castling_moves = piece.get_castling_destinations(&board);
                    castling_moves.retain(|(king_to, _, rook_from, rook_to)| {
                        board.move_piece(drawn_moves_piece, *king_to);
                        board.move_piece(*rook_from, *rook_to);
                        let blocked = board.is_in_check(board.current_player);
                        board.move_piece(*king_to, drawn_moves_piece);
                        board.move_piece(*rook_to, *rook_from);
                        if blocked {
                            blocked_moves.push(*king_to);
                        }
                        return !blocked;
                    });

                    // if board.game_state == GameState::Nominal && moves.len() == 0 && capturing_moves.len() == 0 && capturing_moves.len() == 0 {
                    //     if board.current_player == Faction::White && board.is_in_check(Faction::White) {
                    //         board.game_state = GameState::WhiteCheckmated;
                    //     } else if board.current_player == Faction::Black && board.is_in_check(Faction::Black) {
                    //         board.game_state = GameState::BlackCheckmated; 
                    //     } else {
                    //         board.game_state = GameState::Statemate;
                    //     }
                    //     board.events.push(Event::GameEnded);
                    // }

                    maybe_moves = Some(moves);
                    maybe_castling_moves = Some(castling_moves);
                    maybe_capturing_moves = Some(capturing_moves);
                    maybe_blocked_moves = Some(blocked_moves);
                }
            }
        }

        

        if let (Some(moves), Some(castling_moves), Some(capturing_moves), Some(blocked_moves)) = (&maybe_moves, &maybe_castling_moves, &maybe_capturing_moves, maybe_blocked_moves) {
            draw_moves(&board, moves.iter().cloned(), Color::from_hex(0x7df5b3));
            draw_moves(&board, capturing_moves.iter().cloned(), Color::from_hex(0x4287f5));
            draw_moves(&board, castling_moves.iter().map(|(king_to, _, _, _)| king_to.clone()), Color::from_hex(0xda42f5));
            draw_moves(&board, blocked_moves.into_iter(), Color::from_hex(0xf55442));
        }

        if let Some(held_piece) = maybe_held_piece {
            if let Some(Some(piece)) = board.pieces.get(board.get_piece_index(held_piece.position)) {
                match board.current_player {
                    Faction::White => draw_texture_ex(textures.get_texture(*piece), mouse_position.x - 50.0 + held_piece.offset.x, mouse_position.y - 50.0 + held_piece.offset.y, WHITE, DrawTextureParams {dest_size: Some(Vec2::splat(100.0)), ..Default::default()}),
                    Faction::Black => draw_texture_ex(textures.get_texture(*piece), mouse_position.x - 50.0 + held_piece.offset.x, mouse_position.y - 50.0 + held_piece.offset.y, WHITE, DrawTextureParams {dest_size: Some(Vec2::splat(100.0)), ..Default::default()}),
                }
            }
        }

        let mut maybe_current_move: Option<Move> = None;
        if let (Some(held_piece), true) = (maybe_held_piece, !is_mouse_button_down(MouseButton::Left)) {
            if let (Some(position), Some(moves), Some(castling_moves), Some(capturing_moves), true) = (maybe_position, &maybe_moves, &maybe_castling_moves, &maybe_capturing_moves, held_piece.piece.get_faction() == board.current_player) {
                if moves.contains(&position) {
                    board.move_piece(held_piece.position, position);
                    maybe_current_move = Some(Move::Move(held_piece.piece, held_piece.position, position));
                } else if let Some((king_to, rook, rook_from, rook_to)) = castling_moves.iter().filter(|(king_to, _, _, _)| *king_to == position).next() {
                    board.move_piece(held_piece.position, position);
                    board.move_piece(*rook_from, *rook_to);
                    maybe_current_move = Some(Move::Castle(held_piece.piece, held_piece.position, *king_to, *rook, *rook_from, *rook_to));
                } else if capturing_moves.contains(&position) {
                    if let Some(captured_piece) = board.move_piece(held_piece.position, position) {
                        maybe_current_move = Some(Move::Capture(held_piece.piece, held_piece.position, position, captured_piece));
                    }
                }
            }
            maybe_held_piece = None;
        }

        if let Some(current_move) = &maybe_current_move {
            board.events.push(Event::Move(*current_move))
        }

        if let Some(current_move) = maybe_current_move {
            match current_move {
                Move::Move(Piece::WhiteKing, _, _) => board.block_white_castle(),
                Move::Castle(Piece::WhiteKing, _, _, _, _, _) => board.block_white_castle(),
                Move::Move(Piece::WhiteRook, UVec2 {x: 7, y: 7}, _) => board.block_white_king_side_castle(),
                Move::Move(Piece::WhiteRook, UVec2 {x: 0, y: 7}, _) => board.block_white_queen_side_castle(),
                Move::Move(Piece::BlackKing, _, _) => board.block_black_castle(),
                Move::Castle(Piece::BlackKing, _, _, _, _, _) => board.block_black_castle(),
                Move::Move(Piece::BlackRook, UVec2 {x: 7, y: 0}, _) => board.block_black_king_side_castle(),
                Move::Move(Piece::BlackRook, UVec2 {x: 0, y: 0}, _) => board.block_black_queen_side_castle(),
                _ => ()
            }
        }

        if auto_undo.is_waiting() {
            auto_undo.wait(get_frame_time());
        }

        if is_key_pressed(KeyCode::R) && !auto_undo.is_waiting() && board.events.len() > 0 {
            auto_undo = AutoUndo::Waiting(auto_undo_delay);
        }

        if is_key_pressed(KeyCode::Z) && !auto_undo.is_waiting() || auto_undo.is_ready() {
            if board.events.len() > 0 {
                board.switch_player();
            }
            board.undo();
            if auto_undo.is_ready() && board.events.len() > 0 {
                auto_undo = AutoUndo::Waiting(auto_undo_delay);
            } else if auto_undo.is_ready() {
                auto_undo = AutoUndo::Idle;
            }
        }

        if maybe_current_move.is_some() {
            if board.is_in_check(board.current_player) {
                panic!("Moved into check (SHOULD BE IMPOSSIBLE)");
            }
            board.switch_player();
        }

        fn draw_text_top(message: &str, color: Color) {
            let size = measure_text(message, None, 100, 1.0);
            draw_text(message, screen_width() / 2.0 - size.width / 2.0, size.height + 20.0, 100.0, color);
        }

        match board.game_state {
            GameState::Nominal => (),
            GameState::WhiteCheckmated => draw_text_top("Black Wins!", BLACK),
            GameState::BlackCheckmated => draw_text_top("White Wins!", WHITE),
            GameState::Statemate => draw_text_top("Stalemate", GRAY),
        }
        
        next_frame().await;
    }
}