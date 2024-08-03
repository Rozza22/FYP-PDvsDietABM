#!/usr/bin/env python3
import os
import glob


def main():

    # Call the function to do any preprocessing/generate the model files
    model_files = generate_model_files()

    output_dir = "outputs/outputs_newcalibration"
    os.makedirs(output_dir, exist_ok=True)

    # Start 
    for index, model_file in enumerate(model_files):
        # Create a subdirectory for each configuration to store its output files
        # config_output_dir = os.path.join(output_dir, f"config_{index}")
        # os.makedirs(config_output_dir, exist_ok=True)

        # Command that will be run - this is the same as what you would use on the command line - you can modify this command to use mpirun if you want to
        # run_command = f'python3 main.py {model_file} {config_output_dir}'
        run_command = f'python3 main.py {model_file}'

        # Run the command
        print("Starting model run for " + model_file)
        os.system(run_command)

    # If you want to do any post-processing of results, it could go here
    process_output()

    print("Done")


# Returns a list of filenames which are the model.yaml files you want to run
def generate_model_files():
    # If you want to generate model files dynamically, you could do it here before the models start running.
    # For now, I've just manually listed some files.
    # You could list multiple different model files here to run different models. You could also
    # generate this list programatically if you wanted to.
    # model_files = glob.glob("props/validation/*.yaml")
    # model_files = ["props/model.yaml"]
    # model_files = glob.glob("props/calibration/model*.yaml") # this one is for generating calibration results
    # model_files = glob.glob("props/calibration2/model*.yaml") # this one is for generating best calibrated results
    model_files = glob.glob("props/interventionVariation/model*.yaml") # this one is for generating intervention Variation results
    # model_files = glob.glob("props/validation/model*.yaml") # this one is for generating validation results

    return model_files


# This function could do some statistical analysis or plot some graphs
def process_output():
    print("Processing output")


# If this python script is being called from the command line, run the main function
if __name__ == "__main__":
    main()
