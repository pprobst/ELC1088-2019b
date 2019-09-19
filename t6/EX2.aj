package br.ufsm.lpbd.banking.aspect;

import br.ufsm.lpbd.banking.core.Account;

/*  Aspecto de logging para cada transação realizada em Account.
 * Para cada operação realizada, imprime
 * 	    - nome do método (before)
 *		- número da conta (before)
 *		- quantia a ser transacionada (before)
 *     - valor do saldo da conta (after)
 */
public aspect EX2 {
	pointcut logTransaction(Account acc, float quant) : (execution(* Account.debit(float))
											|| execution(void Account.credit(float))) 
											&& args(quant) && target(acc);
	
	before (Account acc, float quant) : logTransaction(acc, quant) {
		String methodName = thisJoinPointStaticPart.getSignature().getName();
		int accNumber = acc.getAccountNumber();
		float quantTransf = quant;
		System.out.println("> (EX2) Nome do método: " + methodName + 
										"\n" + "    - Conta: " + accNumber +
										"\n" + "    - Transferência: " + quantTransf);
	}
	
	after (Account acc, float quant) : logTransaction(acc, quant) {
		float saldoAcc = acc.getBalance();
		System.out.println("   - Saldo (EX2 AFTER) " + saldoAcc); // isso aparece fora de ordem
	}
}
