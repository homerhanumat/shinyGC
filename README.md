## Shiny Apps for Teaching (Georgetown College)

This repository is a collection of Shiny apps teaching, mostly in introductory statistics classes for non-majors.  It is posted here for easy deployment to the College Shiny Server, and to provide examples for colleagues to use and modify.

Some apps were written a while back when I was just learning my way around Shiny.  They will be re-factored someday; in the meantime I don't recommend emulating their coding strategies.

Reasonably good code can be found in:

* CIMean (explore confidence interval using simulation)
* FindRegLine (regression line game that records students' scores)
* GetToKnow (survey app, souped up with Google maps and basic descriptive statistics)
* SlowAssoc (simulation-based introduction to inference, in the context of the chi-square test for association; uses the new `rhandsontable` package to allow the user to enter data into a two-way table)
* accidents (based on an app in [Mastering Shiny](https://mastering-shiny.org/))

To run an app in your browser with Web Assembly:

>https:homerhanumat.github.io/<app-name>

For example:

>{https:homerhanumat.github.io/accidents}(https:homerhanumat.github.io/accidents)