# Code 1 

## Mappings Should Use Unions

Mapping types are easier to use when the Match type that underlies them use unions when the underlying type is the same. As an example, imagine the platform dependendent type `Foo`.

`Foo` is defined as such for the following platforms:
* WinX64 -> 32-bit Integer
* LinuxX64 -> 64-bit Integer
* MacX64 -> 64-bit Integer

The mapping for this should look as such:

```scala
NumericMapping.create[[T <: Target] => T match 
  case LinuxX64.type | MacX64.type => Long
  case WinX64.type => Int 
]
```