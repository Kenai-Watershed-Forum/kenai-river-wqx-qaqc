

# define function
set_column_classes <- function(data_frame, classes) {
  # Convert the classes to a named vector
  class_vector <- unlist(classes)
  
  # Loop through the names and assign the classes
  for (col_name in names(class_vector)) {
    class_to_set <- class_vector[col_name]
    if (is.na(class_to_set)) {
      class_to_set <- class(data_frame[[col_name]])
    }
    if (class_to_set == "factor" & class(data_frame[[col_name]]) != "factor") {
      data_frame[[col_name]] <- factor(data_frame[[col_name]])
    } else {
      data_frame[[col_name]] <- as(data_frame[[col_name]], class_to_set)
    }
  }
  
  # Return the modified data frame
  return(data_frame)
}



# Define the classes to set
class_list <- list(
  # project_id  (assigned within CDX app)
  monitoring_location_id = "integer",
  activity_media_name = "character",             
  activity_media_subdivision_name = "character",     
  activity_id = "character",                         
  activity_start_date = "character",                  
  activity_start_time = "character",                 
  activity_start_time_zone = "character",            
  activity_latitude = "numeric",                    
  activity_longitude = "numeric",                  
  activity_type = "character",                        
  activity_depth_height_measure = "integer",        
  activity_depth_height_unit = "character",          
  activity_comment = "character",                      
  activity_horizontal_collection_method = "character",
  activity_horizontal_reference_datum = "character",  
  characteristic_name = "character",                   
  result_analytical_method_id = "character",         
  result_analytical_method_context = "character",     
  result_value = "numeric",                         
  result_unit = "character",                         
  result_qualifier = "character",                    
  result_weight_basis = "character",                  
  result_sample_fraction = "character",              
  method_speciation = "character",                    
  result_value_type = "character",                    
  sample_collection_equipment_name = "character",    
  result_detection_condition = "character",          
  result_status_id = "character",                     
  result_detection_limit_type_1 = "character",       
  result_detection_limit_value_1 = "numeric",       
  result_detection_limit_unit_1 = "character",        
  result_detection_limit_type_2 = "character",       
  result_detection_limit_value_2 = "numeric",        
  result_detection_limit_unit_2 = "character",        
  laboratory_name = "character",                     
  laboratory_comment_text = "character",              
  analysis_start_date = "character",                  
  analysis_start_time = "character",                 
  analysis_start_time_zone = "character",            
  thermal_preservative_used = "character",            
  sample_container_type = "character",               
  sample_container_color = "character",              
  chemical_preservative_used = "character"
)



