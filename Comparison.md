## Ex 1

With lens:

```haskell
ex1 :: [Pet]
ex1 =
  company
  & toListOf (staff . folded . employeePets . folded . filteredBy (petType . only "cat"))
```

Type of optic:

```haskell
toListOf :: forall a s. Getting (Endo [a]) s a -> s -> [a]

_ :: Getting (Endo [Pet]) Company Pet -> Company -> [Pet]
```

See: https://hackage.haskell.org/package/lens-4.19.2/docs/Control-Lens-Fold.html#v:toListOf


With optics:

```haskell
ex1 :: [Pet]
ex1 =
  company
  & toListOf (#staff % folded % #employeePets % folded % filteredBy (#petType % only "cat"))
```

type of optic:

```haskell
toListOf :: forall (k :: OpticKind) (is :: IxList) s a.
Is k A_Fold =>
Optic' k is s a -> s -> [a]

_ :: Optic' A_Fold '[()] Company Pet -> Company -> [Pet]
```

See: https://hackage.haskell.org/package/optics-core-0.4/docs/Optics-Fold.html#v:toListOf

Save for the overloaded labels and composition operator, it's the same. The types are very different: `lens` 
often doesn't refer to any actual optics, leveraging the "transparent" nature of the constituent constraints.

## Ex 2

With `lens`

```haskell
owners :: [String]
owners =
  company ^..
    (staff . folded . reindexed _employeeName selfIndex <. employeePets . folded . petName)
    -- _ :: ((String, String) -> Const (Endo [String]) (String, String))
    --  -> Indexed String String (Const (Endo [String]) String)
    . withIndex
    . to (\(eName, pName) -> pName <> " belongs to " <> eName)

```
