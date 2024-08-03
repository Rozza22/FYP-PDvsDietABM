import repast4py
from repast4py import parameters
from mpi4py import MPI
from NormModel import NormModel

def main():
    # Command line argument parsing
    parser = repast4py.parameters.create_args_parser()
    args = parser.parse_args()
    params = repast4py.parameters.init_params(args.parameters_file, args.parameters)
    params["parameters_file"] = args.parameters_file

    # If multiple MPI ranks have been used, terminate with an error message
    if (MPI.COMM_WORLD.Get_size() > 1):
        if MPI.COMM_WORLD.Get_rank() == 0:
            print(f"Error: This tutorial only supports use of a single MPI rank ({MPI.COMM_WORLD.Get_size()} requested).", file=sys.stderr)
        sys.exit(1)

    #Read random seed from model.props. If there is no random seed in props file, proceed as normal
    repast4py.random.init(params["random.seed"])

    #Set up model
    #repast4py.RepastProcess.init(configFile)

    try:
        model = NormModel(MPI.COMM_WORLD, params)
        model.initialize(); #manange schedule (core)
        model.run()
        #repast4py.RepastProcess.instance().done()
        return 0

    except IOError as ioe:
        print("Error " + ioe)
        repast4py.RepastProcess.instance().done()
        MPI_Abort(MPI_COMM_WORLD, MPI_ERR_UNKNOWN) # @todo - there might be a more appropraite error code to propagate?
        return EXIT_FAILURE

if __name__ == "__main__":
    main()
