Comparison between singleton- en non-singleton components in the cake pattern
-----------------------------------------------------------------------------

source: https://github.com/davidmoten/cake-pattern

Rule of thumb:
 * a singleton component should have an *access point*
 * a non-singleton component should not have an *access point*; it rather is declared as a field
   in classes/traits that use the component.
