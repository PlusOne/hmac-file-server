
class PIDController:
    def __init__(self, kp: float, ki: float, kd: float, dt: float, integral_limit: float = None):
        """
        Initialize the PID controller with tuning parameters.

        Args:
            kp (float): Proportional gain
            ki (float): Integral gain
            kd (float): Derivative gain
            dt (float): Time step in seconds
            integral_limit (float, optional): Maximum absolute value for the integral term
        """
        self.kp = kp
        self.ki = ki
        self.kd = kd
        self.dt = dt
        self.integral_limit = integral_limit

        # Internal state
        self.prev_error = 0.0
        self.integral = 0.0

    def compute(self, setpoint: float, process_variable: float) -> float:
        """
        Compute the control signal based on the setpoint and current process variable.

        Args:
            setpoint (float): Desired target value
            process_variable (float): Current measured value

        Returns:
            float: Control signal output
        """
        error = setpoint - process_variable
        self.integral += error * self.dt

        # Integral windup clamping
        if self.integral_limit is not None:
            self.integral = max(min(self.integral, self.integral_limit), -self.integral_limit)

        derivative = (error - self.prev_error) / self.dt

        # PID calculation
        control_signal = (
            self.kp * error +
            self.ki * self.integral +
            self.kd * derivative
        )

        self.prev_error = error
        return control_signal