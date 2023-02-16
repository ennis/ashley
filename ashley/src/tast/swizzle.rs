//! Component access & swizzle checks
use smallvec::SmallVec;

pub(crate) enum ComponentSyntaxError {
    InvalidSyntax,
    TooManyComponents,
}

pub(crate) type ComponentIndices = SmallVec<[i32;4]>;

/// Converts a component selection syntax into corresponding component indices.
///
/// This is done according to the [GLSL specification](https://registry.khronos.org/OpenGL/specs/gl/GLSLangSpec.4.60.html#vector-components)
///
/// # Arguments
///
/// * components a string representing the components to select
/// * num_components number of components in the source type
///
/// # Return value
///
/// The component indices, or `None` if the string was not a valid component selection syntax.
///
/// # Examples
///
/// * "rgba" => `vec![0,1,2,3]`
/// * "yyxx" => `vec![1,1,2,2]`
pub(crate) fn get_component_indices(selection: &str, num_components: usize) -> Result<ComponentIndices, ComponentSyntaxError> {
    let check_component_name_set =
        |names: &str| -> Option<ComponentIndices> { selection.chars().map(|c| names.find(c).map(|i| i as i32)).collect() };

    let v = check_component_name_set("rgba")
        .or_else(|| check_component_name_set("xyzw"))
        .or_else(|| check_component_name_set("stpq"));

    if let Some(v) = v {
        if v.len() < num_components {
            Ok(v)
        } else {
            Err(ComponentSyntaxError::TooManyComponents)
        }
    } else {
        Err(ComponentSyntaxError::InvalidSyntax)
    }
}
