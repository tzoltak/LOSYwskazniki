# LOSYwskazniki 0.2.0 (21.11.2025)

## Naprawione błędy

-   `dodaj_wskazniki_prace()` oblicza `sr_wynagr_uop_nauka_r0_wrzgru` i `sr_wynagr_uop_bez_nauki_r0_wrzgru` jako **względne** wynagrodzenia (w odniesieniu do śr. miesięcznych wynagrodzeń w powiecie zamieszkania w danym roku).

## Nowe funkcje

-   `oblicz_wynagrodzenia_wzgledne()` pozwala obliczyć wskaźniki względnych wynagrodzeń (w odniesieniu do śr. miesięcznych wynagrodzeń w powiecie zamieszkania w danym roku); jej wyodrębnienie wynika z chęci umożliwienia jej użycia w ramach pakietu *MLASdaneAdm*.

# LOSYwskazniki 0.1.3 (17.11.2025)

## Naprawione błędy

-   `dodaj_wskazniki_kontynuacje()` odnajduje informacje o nauce na KKZ i KUZ (w przypadku KKZ wykluczając te, związane z nauką jako uczeń w BS II).
-   `dodaj_wskazniki_kontynuacje()` poprawnie odfiltrowuje tylko *legalne* kontynuacje, biorąc pod uwagę również KKZ i KUZ.

# LOSYwskazniki 0.1.2 (14.10.2025)

-   `dodaj_wskazniki_prace()` oblicza wskaźniki opisujące wynagrodzenia z zatrudnienia w formie umowy o pracę od września do grudnia roku ukończenia szkoły w podziale na okresy kontynuacji i niekontynuowania nauki.

# LOSYwskazniki 0.1.1 (25.08.2025)

-   `dodaj_wskazniki_prace()` oblicza również wskaźnik opisujący wynagrodzenia w I kwartale roku następującego po roku ukończenia szkoły.

# LOSYwskazniki 0.1.0 (08.08.2025)

-   Pierwsza działająca wersja pakietu.
