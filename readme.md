# XLSFormTools

XLSFormTools is a small R package containing several utilities for generating and working with [XLSForm](https://xlsform.org/en/) surveys.

## The Tools

### A Domain Specific Language for Generating XLSForms

Currently XLSForms are generated in Excel, which is unfriendly with respect to both version control and collaboration (e.g. sheets often end up with names like "household_survey_2019_02_01_ewing_v5_final.xlsx"). Writing surveys in a dedicated language embedded in R solves both of these problems by enabling the use of version tools that work well with plain text like Git. Further, embedding the language in R allows users to fully leverage R to generate surveys. For example, a user could generate a survey asking respondents about current events by scraping headlines from news sites each day.

The language is built to match the [XLSForm standard](https://xlsform.org/en/ref-table/) as closely as possible, with the philosophy that if you think something should work, it will. Much of the code is drawn from Hadley Wickham's __Advanced R__ [domain specific language examples](http://adv-r.had.co.nz/dsl.html). Here is an example.

```{r}
# Surveys are defined in their own environment
to_survey({
  # R Syntax and objects are inherited by this environment.
  
  # Settings can be set:
  setting(form_title, "example_form")
  setting(version, "1")
  
  # Choices can be defined, in a single language by passing a single string, 
  choice("yn", "0" = "No", "1" = "Yes")
  # or in multiple languages by using a named vector.
  # choice("yn",
  #        "0" = c(English = "No", Spanish = "No"), 
  #        "1" = c(English = "Yes", Spanish = "Si"))
  
  # Question types all have their own function.
  start(name = "survey_start")
  end(name = "survey_end")
  
  # Groups are easy to set up.
  begin_group("section_a", "Section A", {
    int(name = "age", label = "A1. What is your age?")
    select_one("yn", "read", "A2. Can you read a newspaper?", 
               relevant = "${age} > 6")
    int(name = "hh_size", label = "A3. How large is your household?")
  })
  
  # Repeat groups are also easy
  begin_repeat("section_b", "Section B", {
    select_one("yn", "read", "B1. Do you attend school?")
  })
})
```

## TODO

Ordered by priority.

- [ ] Examples and documentation: Docstrings and vignettes should be completed for each tool.
- [ ] Importing choices: The choice sheet of an XLSForm can grow quite large, it might be better to let users import choices from a .csv for existing XLSForm.
- [ ] Testing: Test suite for each tool.
- [ ] Fuzzing: As with [software testing](https://en.wikipedia.org/wiki/Fuzzing), a survey can be tested by specifying what data __should__ look like (e.g. a respondent over 50 should never be asked about a given set of questions), and then testing whether or not data randomly generated from the survey passes these tests.
- [ ] Survey Navigator: A survey navigator built with Shiny.
