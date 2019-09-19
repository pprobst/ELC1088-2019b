package br.ufsm.lpbd.banking.aspect;

import br.ufsm.lpbd.banking.core.Account;
import br.ufsm.lpbd.banking.core.OverdraftAccount;
import br.ufsm.lpbd.banking.exception.InsufficientBalanceException;

/* Aspecto que garanta a seguinte regra:
 * - se um cheque for compensado (conta corrente/poupança)
 *    e não houver saldo suficiente (abaixo do limite de segurança),
 *    porém houver saldo sufic. na conta de empréstimo (overdraft),
 *    o valor do saque é transferido para a conta corrente ou poupança
 *    para que o saque seja realizado.
 *  - se não houver saldo suficiente na conta de empréstimo, 
 *    InsuficcientBalanceEsception é sinalizada.
 */
public aspect EX4 {
	 pointcut checkRule(Account acc, float quant) : execution(* Account.debit(float)) && args(quant) 
	 																		    && target(acc) && !target(OverdraftAccount);
	 
	 void around(Account acc, float quant) throws InsufficientBalanceException : checkRule(acc, quant) {
		 int tax = 100;
		 boolean emprestimo_ok = false;
		 if ((acc.getBalance() - quant) < tax) {
			 float withdraw = (quant + tax) - acc.getBalance();
			 for (Account overdraft : acc.getCustomer().getOverdraftAccounts()) {
				 if (overdraft.getBalance() > withdraw) {
					 // transferência ocorre aqui
					 overdraft.debit(withdraw);
					 acc.credit(withdraw);
					 emprestimo_ok = true;
					 System.out.println("> (EX4) Transação permitida (c/ empréstimo):\n" + 
		 					    "    - Conta: " + acc.getAccountNumber() + "\n" +
		 					    "    - Transação: " + quant + "\n" +
							    "    - Saldo novo: " + acc.getBalance());
					 proceed(acc, quant);
				 }
			 }
			 if (!emprestimo_ok) throw new InsufficientBalanceException("!! (EX4) TRANSAÇÃO NÃO PERMITIDA : LIMITE DE SEGURANÇA ATINGIDO!\n" + 
					  "    >> Conta: " + acc.getAccountNumber() + "\n" + 
					  "    >> Saldo: " + acc.getBalance() + "\n" + 
					  "    >> Transação: " + quant);
		 } else {
			 System.out.println("> (EX4) Transação permitida:\n" + 
					    "    - Conta: " + acc.getAccountNumber() + "\n" +
					    "    - Transação: " + quant + "\n" +
					    "    - Saldo: " + acc.getBalance());
			 proceed(acc, quant);
		 }
	 }
}
