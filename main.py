
from utils.pid_controller import PIDController

def main():
    # Initialize the PID controller
    pid = PIDController(kp=1.0, ki=0.1, kd=0.01, dt=0.1, integral_limit=100.0)

    setpoint = 10.0  # Desired setpoint
    process_variable = 8.0  # Current process value

    # Compute the control signal
    control_signal = pid.compute(setpoint, process_variable)
    print(f"Control Signal: {control_signal}")

if __name__ == "__main__":
    main()