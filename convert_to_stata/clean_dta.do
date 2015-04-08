* ---------------------------
* Clean responses_countries
* ---------------------------
use "Final/responses_countries.dta"

label data ""

ds, not(type string)
local not_strings `r(varlist)'

foreach x of varlist `not_strings' {
    replace `x' = . if `x' == .z
}

ds, has(type string)
local string_vars `r(varlist)'

foreach x of varlist `string_vars' {
    replace `x' = "" if `x' == "NA"
}

save "Final/responses_countries.dta", replace


* ----------------------
* Clean responses_orgs
* ----------------------
use "Final/responses_orgs.dta"

label data ""

ds, not(type string)
local not_strings `r(varlist)'

foreach x of varlist `not_strings' {
    replace `x' = . if `x' == .z
}

ds, has(type string)
local string_vars `r(varlist)'

foreach x of varlist `string_vars' {
    replace `x' = "" if `x' == "NA"
}

save "Final/responses_orgs.dta", replace
