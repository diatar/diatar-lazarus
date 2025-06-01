Ez a DateTimePicker kontrol, része a Lazarus komponenseknek: lazarus/components/datetimectrls/
Linux gtk2 alatt Show() helyett ShowModal() kell - és ennek megfelelően a bezárás is változik.
Két helyen ezért belenyúltam (785. és 3764.sor), ezt a fájlt tehát Linux fordítás előtt be kell másolni a komponenskönyvtárba.
