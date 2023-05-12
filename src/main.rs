use std::marker::PhantomData;

#[derive(Debug)]
enum StorageValueF { }
#[derive(Debug)]
enum VecF { }
#[derive(Debug)]
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

#[derive(Debug)]
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

impl<Fam: ContainerFam> MonContainer<Fam> {
    fn as_u32(&self) -> Option<&Fam::Container<u32>> {
        match self {
            Self::U32(c) => Some(c),
            _ => None
        }
    }
    
    fn mut_u32(&mut self) -> Option<&mut Fam::Container<u32>> {
        match self {
            Self::U32(c) => Some(c),
            _ => None
        }
    }

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
}

type VecT = MonContainer<VecF>;
type StorageValueT = MonContainer<StorageValueF>;

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

fn main() {
    let mut data: VecT = VecT::construct((), StorageTypeName::U32);
    data.mut_u32().unwrap().extend_from_slice(&[0, 1, 23, 42]);
    let result = data.map(VecIndex { index: 3 });
    let len = data.map(VecLen {}).into_inner();

    println!("vec = {data:?}, result = {result:?}; len = {}", len);
}
