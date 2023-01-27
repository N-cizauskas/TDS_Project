ExpandCodes <- function(input_codes) {
  full_list <- NULL
  for (i in 1:length(input_codes)) {
    tmp_codes <- input_codes[i]
    if (any(grepl("-", tmp_codes))) {
      if (gsub("[[:digit:]]+", "", gsub("-.*", "", tmp_codes)) == gsub("[[:digit:]]+", "", gsub(".*-", "", tmp_codes))) {
        recoded_list <- paste0(
          gsub("[[:digit:]]+", "", gsub("-.*", "", tmp_codes)),
          formatC(gsub("[^0-9.-]", "", gsub("-.*", "", tmp_codes)):gsub("[^0-9.-]", "", gsub(".*-", "", tmp_codes)),
            width = 2, flag = "0"
          )
        )
      } else {
        start_id <- which(LETTERS == gsub("[[:digit:]]+", "", gsub("-.*", "", tmp_codes)))
        stop_id <- which(LETTERS == gsub("[[:digit:]]+", "", gsub(".*-", "", tmp_codes)))
        recoded_list <- NULL
        for (icd_letter in LETTERS[start_id:stop_id]) {
          if (icd_letter == gsub("[[:digit:]]+", "", gsub("-.*", "", tmp_codes))) {
            recoded_list <- c(recoded_list, paste0(icd_letter, formatC(gsub("[^0-9.-]", "", gsub("-.*", "", tmp_codes)):99, width = 2, flag = "0")))
          } else {
            if (icd_letter != gsub("[[:digit:]]+", "", gsub(".*-", "", tmp_codes))) {
              recoded_list <- c(recoded_list, paste0(icd_letter, formatC(0:99, width = 2, flag = "0")))
            } else {
              recoded_list <- c(recoded_list, paste0(icd_letter, formatC(0:gsub("[^0-9.-]", "", gsub(".*-", "", tmp_codes)), width = 2, flag = "0")))
            }
          }
        }
      }
    } else {
      recoded_list <- tmp_codes
    }
    full_list <- c(full_list, recoded_list)
  }
  return(full_list)
}
