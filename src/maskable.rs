use crate::streamable_bitmask::*;
use crate::simd_traits::*;
use std::ops::{RangeBounds,Bound};

///
/// A series of bytes of a particular size which can be
/// efficiently searched and turned into masks.
/// 
trait Maskable<T: Copy> {
    type Bitmask: StreamableBitmask;

    ///
    /// Match all bytes with the given value
    /// 
    fn where_eq(self, value: T) -> Self::Bitmask;

    ///
    /// Match all bytes with the given value
    /// 
    fn where_ne(self, value: T) -> Self::Bitmask;

    ///
    /// Match all bytes greater than the given value
    /// 
    fn where_gt(self, min: T) -> Self::Bitmask;

    ///
    /// Match all bytes greater than or equal to the given value
    /// 
    fn where_ge(self, min: T) -> Self::Bitmask;

    ///
    /// Match all bytes less than the given value
    /// 
    fn where_lt(self, max: T) -> Self::Bitmask;

    ///
    /// Match all bytes less than or equal to the given value
    /// 
    fn where_le(self, max: T) -> Self::Bitmask;

    ///
    /// Match all bytes within the given range
    ///
    fn where_within(self, range: impl RangeBounds<T>) -> Self::Bitmask;
}

impl<T: SimdInteger> Maskable<T::LaneType> for T where T::Bitmask: StreamableBitmask {
    type Bitmask = T::Bitmask;

    fn where_eq(self, value: T::LaneType) -> Self::Bitmask { self.eq(Self::splat(value)).bitmask() }
    fn where_ne(self, value: T::LaneType) -> Self::Bitmask { self.ne(Self::splat(value)).bitmask() }
    fn where_gt(self, value: T::LaneType) -> Self::Bitmask { self.gt(Self::splat(value)).bitmask() }
    fn where_ge(self, value: T::LaneType) -> Self::Bitmask { self.ge(Self::splat(value)).bitmask() }
    fn where_lt(self, value: T::LaneType) -> Self::Bitmask { self.lt(Self::splat(value)).bitmask() }
    fn where_le(self, value: T::LaneType) -> Self::Bitmask { self.le(Self::splat(value)).bitmask() }
    fn where_within(self, range: impl RangeBounds<T::LaneType>) -> Self::Bitmask {
        use Bound::*;
        match (range.start_bound(), range.end_bound()) {
            (Unbounded, Unbounded) => return <Self::Bitmask>::ALL_BITS,
            (Unbounded, Included(max)) => self.where_le(*max),
            (Unbounded, Excluded(max)) => self.where_lt(*max),
            (Included(min), Unbounded) => self.where_ge(*min),
            (Excluded(min), Unbounded) => self.where_gt(*min),
            (Included(min), Included(max)) => self.where_ge(*min) & self.where_le(*max),
            (Included(min), Excluded(max)) => self.where_ge(*min) & self.where_lt(*max),
            (Excluded(min), Included(max)) => self.where_gt(*min) & self.where_le(*max),
            (Excluded(min), Excluded(max)) => self.where_gt(*min) & self.where_lt(*max),
        }
    }
}
