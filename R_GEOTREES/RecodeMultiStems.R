
# Recode the stems number in descending order of DBH by IdTree
# and recode the IdStem

# to put in data.table
# and by census
# and with standard variables

Multistems <- Data %>%
  # filter(Multistem & !is.na(Multistem)) %>% # only sure multistems
  filter(Multistem | is.na(Multistem)) %>% # sure and not sure multistems
  # distinct(IdTree, IdStem, DBH, .keep_all = T) %>% # Résolution cas 2: remove duplicates (dupliidstems déjà réglé au dessus)
  mutate(BasalArea = pi*(DBH/2)^2) %>%
  group_by(IdTree) %>%
  arrange(desc(DBH)) %>%
  mutate(Stem.nb = 1:n()) %>% # numérote les stems par ordre décroissant de DBH
  unite(col = "IdStem", c("IdTree", "Stem.nb"), # recode the IdStem
        sep = "_", na.rm = TRUE, remove = F) %>% # numérote les stems par ordre décroissant de DBH
  arrange(desc(DBHcor)) %>%
  fill(Xutm, Yutm, X, Y,
       Guyafor.nb,
       ScientificName, Genus, Species, Family,
       .direction = "downup") %>%
  ungroup()

data <- Data %>%
  filter(!(Multistem | is.na(Multistem))) %>% # supprimer les Multistem=T ou NA (40997)
  # filter(!Multistem | is.na(Multistem)) %>% # supprimer les Multistem=T (40997)
  mutate(DBHcor = DBH) %>%
  bind_rows(Multistems)

nrow(data)== nrow(Data)
