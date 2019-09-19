package br.ufsm.lpbd.banking.aspect;

import br.ufsm.lpbd.banking.core.Account;
import br.ufsm.lpbd.banking.exception.InsufficientBalanceException;

// ##### Como este aspecto foi reimplementado em EX4, decidi comentar tudo aqui. #####

/* Aspecto que garanta que as transações de débito não permitam que
 * o saldo fique menor que o limite de segurança de R$100,00. 
 * Caso isso ocorra, sinalizar exceção InsufficientBalanceException.
 */
/*
public aspect EX3 {
	 pointcut checkLimit(Account acc, float quant) : execution(* Account.debit(float)) && args(quant) 
	 																			  && target(acc) && !target(OverdraftAccount);
	 
	 void around(Account acc, float quant) throws InsufficientBalanceException : checkLimit(acc, quant) {
		 int tax = 100;
		 if ((acc.getBalance() - quant) < tax) {
			 throw new InsufficientBalanceException("!! (EX3) TRANSAÇÃO NÃO PERMITIDA : LIMITE DE SEGURANÇA ATINGIDO!\n" + 
					 														  "    >> Conta: " + acc.getAccountNumber() + "\n" + 
					 														  "    >> Saldo: " + acc.getBalance() + "\n" + 
					 														  "    >> Transação: " + quant);
		 }
		 else {
			 System.out.println("> (EX3) Transação permitida:\n" + 
					 					    "    - Conta: " + acc.getAccountNumber() + "\n" +
					 					    "    - Transação: " + quant);
			 proceed(acc, quant);
		 }
	 }
}
*/
