# /setup/global/inputs.R

sample_merge_cols <- c(
  "Subject",
  "Date",
  "Bristol",
  "Study_Day",
  "Subj_Day",
  "Subject_Code",
  "Sex",
  "Housing",
  "Group",
  "Age",
  "Contraception",
  "Origin",
  "Previous_Exp",
  "Diet_Trial",
  "Group_Phase",
  "Room",
  "Cage",
  "Housing",
  "Subj_DOB",
  "Age"
)

subjects <- list(
  marmoset = list(
    HAM  = "Hamlet",
    HER  = "Hera",
    JAR  = "JarJar BINKS",
    OPH  = "Ophelia",
    KUB  = "Kubo",
    KOR  = "Korra",
    WOL  = "Wolverine",
    IRI  = "Iris",
    GOO  = "Goose",
    LAM  = "Lambchop",
    FRA  = "Franc",
    IVY  = "Ivy",
    CHA  = "Charles",
    PAD  = "Padme",
    GRO  = "Grogu",
    MAR  = "Marshmallow",
    BUD  = "Buddy",
    JOA  = "Joans",
    HEN  = "Henry",
    GIN  = "Ginger"
  )
)

taxonomy.ordered <- c(
  "superkingdom",
  "phylum",
  "class",
  "order",
  "family",
  "genus",
  "species"
)

seqruns <- list(
  marmoset  = as.list(sprintf("cm%03d", 1:14))
)

marm_foods <- list(
  "Zupreem^\u00AE Marmoset Diet" = list(
    "Commercially formulated diet made up of wheat, soybean, egg, alfalfa, and various vitamins",
    1,
    "slice"
  ),
  "Mazuri^\u00AE Callitrichid Gel Diet" = list(
    "Commercially formulated diet made up of soybean, corn, wheat, gelatin, and various vitamins",
    1,
    "slice"
  ),
  "Apple" = list(
    "Cut into 1-cm^2 cubes",
    4,
    "pieces"
  ),
  "Sweet Potato" = list(
    "Cut into 1-cm^2 cubes",
    4,
    "pieces"
  ),
  "Eggs" = list(
    "Scrambled and microwave-cooked",
    2,
    "tsp"
  )
)

dataset.names <- list(
  "main",
  "group1",
  "group2",
  "baseline",
  "gum",
  "gumwash",
  "ctrlwash",
  "int",
  "ne",
  "tx",
  ""
)