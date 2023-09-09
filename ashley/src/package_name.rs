use std::{borrow::Cow, fmt};

/// Arguments of a package name.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PackageArg<'a> {
    /// String argument.
    String(Cow<'a, str>),
    /// Integer argument.
    Int(i32),
}

impl fmt::Display for PackageArg<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PackageArg::String(s) => write!(f, "{s:?}"),
            PackageArg::Int(i) => write!(f, "{i}"),
        }
    }
}

impl<'a> From<&'a str> for PackageArg<'a> {
    fn from(s: &'a str) -> Self {
        PackageArg::String(Cow::Borrowed(s))
    }
}

impl From<String> for PackageArg<'_> {
    fn from(s: String) -> Self {
        PackageArg::String(Cow::Owned(s))
    }
}

impl From<i32> for PackageArg<'_> {
    fn from(i: i32) -> Self {
        PackageArg::Int(i)
    }
}

/// Package name. Composed of a base name and a list of arguments.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleName<'a> {
    pub base_name: Cow<'a, str>,

    // Can't use SmallVec because of a variance issue: https://github.com/servo/rust-smallvec/issues/146.
    // At least it won't allocate by default.
    //pub args: SmallVec<[PackageArg<'a>; 2]>,
    pub args: Vec<PackageArg<'a>>,
}

impl fmt::Display for ModuleName<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}(", self.base_name)?;
        for (i, arg) in self.args.iter().enumerate() {
            if i > 0 {
                write!(f, ",")?;
            }
            write!(f, "{}", arg)?;
        }
        write!(f, ")")?;
        Ok(())
    }
}

impl<'a> ModuleName<'a> {
    pub fn new(base_name: impl Into<Cow<'a, str>>) -> Self {
        // TODO: validate name
        Self {
            base_name: base_name.into(),
            args: vec![],
        }
    }

    pub fn arg(mut self, arg: impl Into<PackageArg<'a>>) -> Self {
        self.args.push(arg.into());
        self
    }

    pub fn into_static(self) -> ModuleName<'static> {
        ModuleName {
            base_name: Cow::Owned(self.base_name.into_owned()),
            args: self
                .args
                .into_iter()
                .map(|arg| match arg {
                    PackageArg::String(s) => PackageArg::String(Cow::Owned(s.into_owned())),
                    PackageArg::Int(i) => PackageArg::Int(i),
                })
                .collect(),
        }
    }
}

impl<'a> From<&'a str> for ModuleName<'a> {
    fn from(s: &'a str) -> Self {
        Self::new(s)
    }
}

impl From<String> for ModuleName<'_> {
    fn from(s: String) -> Self {
        Self::new(s)
    }
}
