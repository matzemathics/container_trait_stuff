use std::marker::PhantomData;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum StorageValueF { }
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum VecF { }
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum OptionF { }

trait StorageValueType : Default + Copy {}

impl StorageValueType for u32 {}
impl StorageValueType for u64 {}

trait ContainerFam {
    type Container<T>;
    type ConstructorArguments;

    fn construct<T: StorageValueType>(args: Self::ConstructorArguments) -> Self::Container<T>;
}

impl ContainerFam for VecF {
    type Container<T> = Vec<T>;
    type ConstructorArguments = ();

    fn construct<T: StorageValueType>(_args: ()) -> Self::Container<T> {
        Vec::new()
    }
}

impl ContainerFam for StorageValueF {
    type Container<T> = T;
    type ConstructorArguments = ();

    fn construct<T: StorageValueType>(_args: ()) -> Self::Container<T> {
        T::default()
    }
}

impl ContainerFam for OptionF {
    type Container<T> = Option<T>;
    type ConstructorArguments = ();

    fn construct<T: StorageValueType>(_args: ()) -> Self::Container<T> {
        None
    }
}

enum StorageTypeName {
    U32,
    U64
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum MonContainer<F: ContainerFam> {
    U32(F::Container<u32>),
    U64(F::Container<u64>)
}

trait ContainerOpMut<Fam: ContainerFam> {
    type OpRes: ContainerFam;
    fn apply_mut<T: StorageValueType>(self, input: &mut Fam::Container<T>) -> 
        <Self::OpRes as ContainerFam>::Container<T>;
}

trait ContainerOp<Fam: ContainerFam>: ContainerOpMut<Fam> {
    fn apply<T: StorageValueType>(self, input: &Fam::Container<T>) -> 
        <Self::OpRes as ContainerFam>::Container<T>;
}

trait ContainerFrom<Fam: ContainerFam> : ContainerFam {
    type FromArgs;

    fn build_from<T: StorageValueType>(c: Fam::Container<T>, args: Self::FromArgs) -> Self::Container<T>;
}

trait ContainerAdd : ContainerFam {
    fn add<T: StorageValueType>(c: &mut Self::Container<T>, elem: T);
}

trait ContainerOf<T> {
    type Family: ContainerFam;
    fn as_inner(&self) -> Option<&<Self::Family as ContainerFam>::Container<T>>;
    fn mut_inner(&mut self) -> Option<&mut <Self::Family as ContainerFam>::Container<T>>;
    fn into_inner(self) -> Option<<Self::Family as ContainerFam>::Container<T>>;
}

macro_rules! mon_container_of_impl {
    ($variant:ident => $ty:ty) => {
        impl<Fam: ContainerFam> ContainerOf<$ty> for MonContainer<Fam> {
            type Family = Fam;

            fn as_inner(&self) -> Option<&Fam::Container<$ty>> {
                match self {
                    Self::$variant(c) => Some(c),
                    _ => None
                }
            }

            fn mut_inner(&mut self) -> Option<&mut Fam::Container<$ty>> {
                match self {
                    Self::$variant(c) => Some(c),
                    _ => None
                }
            }

            fn into_inner(self) -> Option<Fam::Container<$ty>> {
                match self {
                    Self::$variant(c) => Some(c),
                    _ => None
                }
            }
        }       
    };
}

mon_container_of_impl!(U32 => u32);
mon_container_of_impl!(U64 => u64);

impl<Fam: ContainerFam> MonContainer<Fam> {
    fn map<Op: ContainerOp<Fam>>(&self, mapping: Op) -> MonContainer<Op::OpRes> {
        match self {
            Self::U32(c) => 
                MonContainer::U32(mapping.apply(c)),
            Self::U64(c) =>
                MonContainer::U64(mapping.apply(c))
        }
    }

    fn get_type(&self) -> StorageTypeName {
        match self {
            MonContainer::U32(_) => StorageTypeName::U32,
            MonContainer::U64(_) => StorageTypeName::U64,
        }
    }

    fn construct(args: Fam::ConstructorArguments, ty: StorageTypeName) -> Self {
        match ty {
            StorageTypeName::U32 => Self::U32(Fam::construct(args)),
            StorageTypeName::U64 => Self::U64(Fam::construct(args)),
        }
    }

    fn build_into<F: ContainerFrom<Fam>>(self, args: F::FromArgs) -> MonContainer<F> {
        match self {
            MonContainer::U32(c) => MonContainer::U32(F::build_from(c, args)),
            MonContainer::U64(c) => MonContainer::U64(F::build_from(c, args)),
        }
    }
}

impl<F: ContainerAdd> MonContainer<F> {
    fn add(&mut self, elem: StorageValueT) {
        match self {
            MonContainer::U32(c) => {
                let StorageValueT::U32(v) = elem else { panic!("wrong value type") };
                F::add(c, v);
            }
            MonContainer::U64(c) => {
                let StorageValueT::U64(v) = elem else { panic!("wrong value type") };
                F::add(c, v);
            }
        }
    }
}

type VecT = MonContainer<VecF>;
type StorageValueT = MonContainer<StorageValueF>;
type OptionT = MonContainer<OptionF>;

struct ConstF<T> {
    _phantom: PhantomData<T>
}

impl<T> ContainerFam for ConstF<T> {
    type Container<TT> = T;
    type ConstructorArguments = T;

    fn construct<TT: StorageValueType>(args: Self::ConstructorArguments) -> Self::Container<TT> {
        args
    }
}

impl<T> MonContainer<ConstF<T>> {
    fn into_inner(self) -> T {
        match self {
            MonContainer::U32(v) => v,
            MonContainer::U64(v) => v,
        }
    }
}

impl TryFrom<StorageValueT> for u32 {
    type Error = ();

    fn try_from(value: StorageValueT) -> Result<Self, Self::Error> {
        match value {
            MonContainer::U32(v) => Ok(v),
            _ => Err(())
        }
    }
}

impl TryFrom<StorageValueT> for u64 {
    type Error = ();

    fn try_from(value: StorageValueT) -> Result<Self, Self::Error> {
        match value {
            MonContainer::U64(v) => Ok(v),
            _ => Err(())
        }
    }
}

macro_rules! const_op {
    ($name:ident = $fam:ident::$op:ident($($param:ident : $param_ty:ty),*)$(.$post_op:tt$post_op_args:tt)* -> $res:ty) => {
        pub struct $name { $($param: $param_ty),* }
        impl ContainerOpMut<$fam> for $name {
            type OpRes = $res;

            fn apply_mut<T: StorageValueType>(self, input: &mut <$fam as ContainerFam>::Container<T>) -> 
                <Self::OpRes as ContainerFam>::Container<T> {
                input.$op($(self.$param),*)$(.$post_op$post_op_args)*
            }
        }
        impl ContainerOp<$fam> for $name {
            fn apply<T: StorageValueType>(self, input: &<$fam as ContainerFam>::Container<T>) -> 
                <Self::OpRes as ContainerFam>::Container<T> {
                input.$op($(self.$param),*)$(.$post_op$post_op_args)*
            }
        }       
    };
}

const_op!(IsSome = OptionF::is_some() -> ConstF<bool>);
const_op!(Unwrap = OptionF::unwrap() -> StorageValueF);
const_op!(UnwrapOrDefault = OptionF::unwrap_or_default() -> StorageValueF);
const_op!(VecLen = VecF::len() -> ConstF<usize>);
const_op!(VecGet = VecF::get(index: usize).cloned() -> OptionF);
const_op!(VecIndex = VecF::get(index: usize).cloned().unwrap() -> StorageValueF);

impl ContainerAdd for VecF {
    fn add<T: StorageValueType>(c: &mut Self::Container<T>, elem: T) {
        c.push(elem);
    }
}

fn main() {
    let mut data: VecT = VecT::construct((), StorageTypeName::U32);

    for d in [0, 1, 23, 41] {
        data.add(StorageValueT::U32(d))
    }

    ContainerOf::<u32>::mut_inner(&mut data).unwrap()[3] += 1;

    let result = data.map(VecGet { index: 3 });
    let result1 = result;
    let len = data.map(VecLen {}).into_inner();

    assert_eq!(ContainerOf::<u32>::as_inner(&data).unwrap(), &[0, 1, 23, 42]);
    assert_eq!(result, OptionT::U32(Some(42)));
    assert_eq!(result, result1);
    assert_eq!(len, 4);
}
