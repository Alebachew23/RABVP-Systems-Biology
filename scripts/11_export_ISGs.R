library(writexl)

write_xlsx(
  list(
    One_hour_Antagonized = One_hour_Antagonized_Df,
    Six_hours_Antagonized = Six_hours_Antagonized_Df,
    TwentyFour_hours_Antagonized = TwentyFour_hours_Df,
    One_hour_Unaffected = One_hour_Unaffected_Df,
    Six_hours_Unaffected = Six_hours_Unaffected_Df,
    TwentyFour_hours_Unaffected = TwentyFour_hours_Unaffected_Df
  ),
  path = "RABV-P_Antagonized_Unaffected_genes.xlsx"
)
