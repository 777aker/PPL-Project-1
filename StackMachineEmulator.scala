package edu.colorado.csci3155.project1


/* -- Here are all the instructions to be supported --*/
sealed trait StackMachineInstruction
case class LoadI(s: String) extends StackMachineInstruction
case class  StoreI(s: String) extends StackMachineInstruction
case class PushI(f: Double) extends StackMachineInstruction
case object AddI extends StackMachineInstruction
case object SubI extends StackMachineInstruction
case object MultI extends StackMachineInstruction
case object DivI extends StackMachineInstruction
case object ExpI extends StackMachineInstruction
case object LogI extends StackMachineInstruction
case object SinI extends StackMachineInstruction
case object CosI extends StackMachineInstruction
case object PopI extends StackMachineInstruction


object StackMachineEmulator {



    /* Function emulateSingleInstruction
        Given a list of doubles to represent a stack
              a map from string to double precision numbers for the environment
        and   a single instruction of type StackMachineInstruction
        Return a tuple that contains the
              modified stack that results when the instruction is executed.
              modified environment that results when the instruction is executed.

        Make sure you handle the error cases: eg., stack size must be appropriate for the instruction
        being executed. Division by zero, log of a non negative number
        Throw an exception or assertion violation when error happens.
     */
    def emulateSingleInstruction(stack: List[Double],
                                 env: Map[String, Double],
                                 ins: StackMachineInstruction): (List[Double], Map[String, Double]) = {
        // TODO: Your code here.
        ins match {
            case LoadI(str) => {
                if(stack.length < 1) {
                    throw new IllegalArgumentException("Stack has no values to load.")
                }
                val newmap = env + (str -> stack.head)
                (stack.drop(1), newmap)
            }
            case StoreI(str) => {
                if(env.contains(str)) {
                    val newstack = env(str) :: stack
                    (newstack, env)
                } else {
                    throw new IllegalArgumentException(s"Environment doesn't contain $str")
                }
            }
            case PushI(d) => {
                val newstack = d :: stack
                (newstack, env)
            }
            case PopI => {
                if(stack.length < 1) {
                    throw new IllegalArgumentException("Cannot pop empty stack.")
                }
                (stack.drop(1), env)
            }
            case AddI => {
                if(stack.length < 2) {
                    throw new IllegalArgumentException("Not enough elements in stack to add.")
                }
                val num1 = stack.head
                val num2 = stack.drop(1).head
                val result = num1 + num2
                val newstack = result :: stack.drop(2)
                (newstack, env)
            }
            case SubI => {
                if(stack.length < 2) {
                    throw new IllegalArgumentException("Not enough elements in stack to subtract.")
                }
                val num1 = stack.head
                val num2 = stack.drop(1).head
                val result = num2 - num1
                val newstack = result :: stack.drop(2)
                (newstack, env)
            }
            case MultI => {
                if(stack.length < 2) {
                    throw new IllegalArgumentException("Not enough elements in stack to multiply.")
                }
                val num1 = stack.head
                val num2 = stack.drop(1).head
                val result = num2 * num1
                val newstack = result :: stack.drop(2)
                (newstack, env)
            }
            case DivI => {
                if(stack.length < 2) {
                    throw new IllegalArgumentException("Not enough elements in stack to divide.")
                }
                val num1 = stack.head
                val num2 = stack.drop(1).head
                if(num1 == 0) {
                    throw new IllegalArgumentException("Can't divide by zero.")
                }
                val result = num2 / num1
                val newstack = result :: stack.drop(2)
                (newstack, env)
            }
            case LogI => {
                if(stack.length < 1) {
                    throw new IllegalArgumentException("Not enough elements in stack to compute log.")
                }
                val num1 = math.log(stack.head)
                if(num1 <= 0) {
                    throw new IllegalArgumentException("Can't compute log of negative number.")
                }
                val newstack = num1 :: stack.drop(1)
                (newstack, env)
            }
            case ExpI => {
                if(stack.length < 1) {
                    throw new IllegalArgumentException("Not enough elements in stack to compute e^x.")
                }
                val newstack = math.exp(stack.head) :: stack.drop(1)
                (newstack, env)
            }
            case SinI => {
                if(stack.length < 1) {
                    throw new IllegalArgumentException("Not enough elements in stack to compute sin(x).")
                }
                val newstack = math.sin(stack.head) :: stack.drop(1)
                (newstack, env)
            }
            case CosI => {
                if(stack.length < 1) {
                    throw new IllegalArgumentException("Not enough elements in stack to compute cos(x)")
                }
                val newstack = math.cos(stack.head) :: stack.drop(1)
                (newstack, env)
            }
        }
    }

    /* Function emulateStackMachine
       Execute the list of instructions provided as inputs using the
       emulateSingleInstruction function.
       Use foldLeft over list of instruction rather than a for loop if you can.
       Return value must be the final environment.

       Hint: accumulator for foldLeft must be a tuple (List[Double], Map[String,Double])
             initial value of this accumulator must be (Nil, Map.empty)
             You should use emulateSingleInstruction to update the accmulator.
             It will all fit nicely once you figure it out.
     */
    def emulateStackMachine(instructionList: List[StackMachineInstruction]): Map[String, Double] =
        {
            //TODO: Your Code here
            val result = instructionList.foldLeft[(List[Double], Map[String, Double])] ((Nil, Map.empty)) ( (acc: (List[Double], Map[String, Double]), instruction: StackMachineInstruction) => emulateSingleInstruction(acc._1, acc._2, instruction) )
            result._2
        }
}