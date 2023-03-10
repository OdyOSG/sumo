#' Function that lists high impact journals for a search
#' @export
highImpactJournals <- function() {
  c(
    #Nature
    'Science','Nature','Nature Medicine',
    'Nature Communications','Nature Digital Health',
    #NEJM
    'The New England journal of medicine',
    # JAMA
    'JAMA', 'JAMA internal medicine','JAMA Surgery','JAMA Psychiatry',
    'JAMA Cardiology','JAMA Pediatrics','JAMA Neurology',
    'JAMA Ophthalmology','JAMA Oncology',
    #BMJ
    'BMJ',
    # Pharmacoep
    'Pharmacoepidemiology and drug safety','Drug safety',
    'Annals of internal medicine',
    #Lancet
    'Lancet', 'The Lancet. Digital health',
    'The Lancet. Infectious diseases','The Lancet. Diabetes endocrinology',
    'The Lancet. Neurology','The Lancet. Oncology',
    'The Lancet. Respiratory medicine','The Lancet. Psychiatry',
    'The Lancet. Global health','The Lancet Digital Health',
    'The Lancet Haematology','The Lancet Gastroenterology & Hepatology',
    # Misc
    'Circulation','Diabetes Care', 'Hypertension',
    'Journal of the American College of Cardiology','Vaccine',
    'Journal of Clinical Oncology', 'Journal of the National Cancer Institute',
    'International Journal of Epidemiology', 'American Journal of Epidemiology',
    #PLOS
    'PLOS Digital Health','PLOS Medicine','PLOS One')
}

#' Function that lists terms for observational study
#' @export
observationalStudy <- function() {
  c("cohort", "case-control", "self-controlled", "retrospective analysis",
    "active comparator", "comparative effectiveness")
}

#' Function that lists common MeSH terms that are unlikely to be useful
#' @export
commonKeywords <- function() {
  c("Humans", "Female", "Male", "Adult", "Middle Aged", "Young Adult",
    "Adolescent", "Aged", "Child", "Child, Preschool", "Infant, Newborn",
    "Infant", "Aged, 80 and over")
}
