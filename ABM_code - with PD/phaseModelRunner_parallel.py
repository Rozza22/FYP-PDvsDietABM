import os
import glob
import concurrent.futures


def main():

    # Call the function to do any preprocessing/generate the model files
    model_files = generate_model_files()

    # Start 
    with concurrent.futures.ProcessPoolExecutor(max_workers=25) as executor:
        # Submit a run_command task for each model file
        futures = [executor.submit(run_command, model_file) for model_file in model_files]

        # Wait for all tasks to complete
        concurrent.futures.wait(futures)

    # If you want to do any post-processing of results, it could go here
    process_output()

    print("Done")


# Returns a list of filenames which are the model.yaml files you want to run
def generate_model_files():
    # If you want to generate model files dynamically, you could do it here before the models start running.
    # For now, I've just manually listed some files.
    # You could list multiple different model files here to run different models. You could also
    # generate this list programatically if you wanted to.
    model_files = glob.glob("props/calibration/*.yaml")

    return model_files


# This function executes the run_command for a single model file
def run_command(model_file):
    # Command that will be run - this is the same as what you would use on the command line - you can modify this command to use mpirun if you want to
    run_command = 'python main.py ' + model_file

    # Run the command
    print("Starting model run for " + model_file)
    os.system(run_command)


# This function could do some statistical analysis or plot some graphs
def process_output():
    print("Processing output")


# If this python script is being called from the command line, run the main function
if __name__ == "__main__":
    main()
