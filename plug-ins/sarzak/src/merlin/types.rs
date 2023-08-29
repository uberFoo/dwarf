//! This is the second iteration of the drawing domain. The first sucked.
//!
//! This domain represents the visual aspect of a model.
// {"magic":"","directive":{"Start":{"directive":"allow-editing","tag":"merlin-module-definition-file"}}}
pub mod anchor;
pub mod bisection;
pub mod bottom;
pub mod edge;
pub mod glyph;
pub mod inflection;
pub mod left;
pub mod line;
pub mod line_segment;
pub mod line_segment_point;
pub mod many;
pub mod one;
pub mod point;
pub mod relationship_name;
pub mod relationship_phrase;
pub mod right;
pub mod sub;
pub mod top;
pub mod x_box;
pub mod x_super;

pub use crate::merlin::anchor::Anchor;
pub use crate::merlin::bisection::Bisection;
pub use crate::merlin::bottom::Bottom;
pub use crate::merlin::bottom::BOTTOM;
pub use crate::merlin::edge::Edge;
pub use crate::merlin::glyph::Glyph;
pub use crate::merlin::glyph::GlyphEnum;
pub use crate::merlin::inflection::Inflection;
pub use crate::merlin::inflection::INFLECTION;
pub use crate::merlin::left::Left;
pub use crate::merlin::left::LEFT;
pub use crate::merlin::line::Line;
pub use crate::merlin::line_segment::LineSegment;
pub use crate::merlin::line_segment_point::LineSegmentPoint;
pub use crate::merlin::many::Many;
pub use crate::merlin::many::MANY;
pub use crate::merlin::one::One;
pub use crate::merlin::one::ONE;
pub use crate::merlin::point::Point;
pub use crate::merlin::point::PointEnum;
pub use crate::merlin::relationship_name::RelationshipName;
pub use crate::merlin::relationship_phrase::RelationshipPhrase;
pub use crate::merlin::right::Right;
pub use crate::merlin::right::RIGHT;
pub use crate::merlin::sub::Sub;
pub use crate::merlin::sub::SUB;
pub use crate::merlin::top::Top;
pub use crate::merlin::top::TOP;
pub use crate::merlin::x_box::XBox;
pub use crate::merlin::x_super::XSuper;
pub use crate::merlin::x_super::X_SUPER;
// {"magic":"","directive":{"End":{"directive":"allow-editing"}}}
