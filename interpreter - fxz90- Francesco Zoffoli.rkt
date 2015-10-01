(load "classParser.scm")
(define pending_instance_ids '())

(define push_ID
  (lambda (id)
    (set! pending_instance_ids (cons id pending_instance_ids))
    id))

(define pull_ID
  (lambda ()
    (let ((id (car pending_instance_ids)))
      (set! pending_instance_ids (cdr pending_instance_ids))
      id)))

(define unique_id
  (let ((next 0))
    (lambda ()
      (let ((v next))
        (set! next (+ next 1))
        (number->string v)))))

(define no_value
  (lambda () 'no_value))

(define contains?
  (lambda (element l)
    (cond
      ((null? l) #f)
      ((eq? (car l) element) #t)
      (else (contains? element (cdr l))))))

(define replace_at_index
  (lambda (val index l)
    (cond
      ((null? l) '())
      ((= (- index (length l)) -1) (cons val (cdr l)))
      (else (cons (car l) (replace_at_index val index (cdr l)))))))

(define get_position_from_end
  (lambda (elem l)
    (cond
      ((null? l) (error "[Env] No element in the list. Element" elem))
      ((eq? (car l) elem) (length (cdr l)))
      (else (get_position_from_end elem (cdr l))))))

(define element_from_end
  (lambda (position l)
    (cond
      ((null? l) (error "[Env] The position is bigger than the list"))
      ((= (+ position 1) (length l)) (car l))
      (else (element_from_end  position (cdr l))))))

(define get_operation_name
  (lambda (stmt) (car stmt)))

(define get_first_argument
  (lambda (stmt) (cadr stmt)))

(define get_second_argument
  (lambda (stmt) (caddr stmt)))

(define get_third_argument
  (lambda (stmt) (cadddr stmt)))

(define get_all_arguments
  (lambda (stmt) (cdr stmt)))

(define convert_from_bool
  (lambda (a)
    (if a 'true 'false)))

(define convert_to_bool
  (lambda (a)
    (cond
      ((eq? a 'true) #t)
      ((eq? a 'false) #f)
      (else (error "[Conversion] No know boolean value for " a)))))

(define make_return
  (lambda (val env)
   (list val env)
    ))

(define get_val_from_return
  (lambda (return)
    (car return)))

(define get_env_from_return
  (lambda (return)
    (cadr return)))

(define make_method
  (lambda (name param code class_context)
    (list name param code class_context)))

(define get_method_name
  (lambda (function)
    (car function)))

(define get_method_param
  (lambda (function)
    (cadr function)))

(define get_method_code
  (lambda (function)
    (caddr function)))

(define get_method_class
  (lambda (function)
    (cadddr function)))

(define make_instance
  (lambda (ID type vals)
    (list ID type vals)))

(define get_instance_ID
  (lambda (instance)
    (car instance)))

(define get_instance_type
  (lambda (instance)
    (cadr instance)))

(define get_instance_vals
  (lambda (instance)
    (caddr instance)))

(define add_val_to_instance
  (lambda (instance val)
    (make_instance (get_instance_ID instance)
                   (get_instance_type instance)
                   (cons val (get_instance_vals instance)))))

(define update_val_to_instance
  (lambda (val position instance)
    (make_instance (get_instance_ID instance)
                   (get_instance_type instance)
                   (replace_at_index val position (get_instance_vals instance)))))

(define get_val_from_instance
  (lambda (position instance)
    (element_from_end position (get_instance_vals instance))))

(define make_class
  (lambda (name parent static_methods static_vars static_vals methods vars vals_default)
    (list name parent static_methods static_vars static_vals methods vars vals_default)))

(define new_class_extends
  (lambda (name parent)
    ;           name
    ;              parent
    ;                      s_meth
    ;                           s_var
    ;                               s_val
    ;                                   meth
    ;                                       var
    ;                                           val defalut
    (make_class name parent '() '() '() '() '() '())))

(define new_class
  (lambda (name)
    (new_class_extends name '())))

(define get_class_name
  (lambda (class)
    (car class)))

(define get_class_parent
  (lambda (class)
    (cadr class)))

(define get_class_static_methods
  (lambda (class)
    (caddr class)))

(define get_class_static_vars
  (lambda (class)
    (cadddr class)))

(define get_class_static_vals
  (lambda (class)
    (car (cddddr class))))

(define get_class_methods
  (lambda (class)
    (caddr (cdddr class))))

(define get_class_vars
  (lambda (class)
    (cadddr (cdddr class))))

(define get_class_vals_default
  (lambda (class)
    (car (cddddr (cdddr class)))))

(define add_static_method_to_class
  (lambda (method class)
    (make_class (get_class_name class) (get_class_parent class)
                (cons method (get_class_static_methods class))
                (get_class_static_vars class)
                (get_class_static_vals class)
                (get_class_methods class)
                (get_class_vars class)
                (get_class_vals_default class))))

(define add_method_to_class
  (lambda (method class)
    (make_class (get_class_name class) (get_class_parent class)
                (get_class_static_methods class)
                (get_class_static_vars class)
                (get_class_static_vals class)
                (cons method (get_class_methods class))
                (get_class_vars class)
                (get_class_vals_default class))))

(define _get_method_by_name
  (lambda (name methods raise?)
    (cond
      ((null? methods) (if raise?
                           (error "[Class] The class has not the method " name)
                           '()))
      ((eq? (get_method_name (car methods)) name) (car methods))
      (else (_get_method_by_name name (cdr methods) raise?))
      )))

(define get_static_method_by_name
  (lambda (method_name class)
    (_get_method_by_name method_name (get_class_static_methods class) #t)))

(define has_static_method?
  (lambda (method_name class)
    (not (null? (_get_method_by_name method_name (get_class_static_methods class) #f)))))

(define get_method_by_name
  (lambda (method_name class)
    (_get_method_by_name method_name (get_class_methods class) #t)))

(define has_method?
  (lambda (method_name class)
    (not (null? (_get_method_by_name method_name (get_class_methods class) #f)))))

(define add_static_var_to_class
  (lambda (var val class)
    (make_class (get_class_name class) (get_class_parent class) (get_class_static_methods class)
                (cons var (get_class_static_vars class))
                (cons val (get_class_static_vals class))
                (get_class_methods class)
                (get_class_vars class)
                (get_class_vals_default class))))

(define add_var_to_class
  (lambda (var val class)
    (make_class (get_class_name class) (get_class_parent class) (get_class_static_methods class)
                (get_class_static_vars class)
                (get_class_static_vals class)
                (get_class_methods class)
                (cons var (get_class_vars class))
                (cons val (get_class_vals_default class)))))

(define append_static_vars_to_class
  (lambda (vars vals class)
    (cond
      ((and (null? vars) (not (null? vals))) (error "The number of vals and vars should be the same. Got " (list vars vals)))
      ((null? vars) class)
      (else (add_static_var_to_class (car vars) (car vals) (append_static_vars_to_class (cdr vars) (cdr vals) class))))))

(define append_vars_to_class
  (lambda (vars vals class)
    (cond
      ((and (null? vars) (not (null? vals))) (error "The number of vals and vars should be the same. Got " (list vars vals)))
      ((null? vars) class)
      (else (add_var_to_class (car vars) (car vals) (append_vars_to_class (cdr vars) (cdr vals) class))))))

(define update_static_var_to_class
  (lambda (var val class_type class)
    (make_class (get_class_name class) (get_class_parent class) (get_class_static_methods class)
                (get_class_static_vars class)
                (replace_at_index val (get_position_from_end var (get_class_static_vars class_type))
                                      (get_class_static_vals class))
                (get_class_methods class)
                (get_class_vars class)
                (get_class_vals_default class))))

(define get_static_var_from_class
  (lambda (var class)
    (element_from_end (get_position_from_end var (get_class_static_vars class)) (get_class_static_vals class))))

(define get_var_position_from_class
  (lambda (var class)
    (get_position_from_end var (get_class_vars class))))

(define has_static_var_in_class?
  (lambda (var class)
    (contains? var (get_class_static_vars class))))

(define has_var_in_inst?
  (lambda (var class)
    (contains? var (get_class_vars class))))

(define error_function
  (lambda (error_message)
    (lambda (v) (error "" error_message v))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define empty_global_environment
  (lambda ()
    ;first parenthesis -> classes
    ; second -> instances
    ; third -> nothing (keept for compatibility with existing code)
    (list '() '() '())
    ))

(define global_environment
  (lambda (classes instances)
    (list classes instances '())
    ))

(define new_scope
  (lambda (vars vals parent)
    (list vars vals parent)))

(define new_empty_scope
  (lambda (parent_env)
    (new_scope '() '() parent_env)))

(define scope_vars
  (lambda (env)
    (car env)))

(define scope_vals
  (lambda (env)
    (cadr env)))

(define pop_scope
  (lambda (env)
    (caddr env)))

(define is_first_scope?
  (lambda (env)
    (null? (caddr env))))

(define get_first_scope
  (lambda (env)
    (if (is_first_scope? env)
        env
        (get_first_scope (pop_scope env)))))

;exectues a function over the global_environment, then rebuild the scopes popped in order to get to the global environment
;The function must take the old global_env as input and return the new global_env as output.
;Used to avoid to rewrite all the logic for this procedure each time I need to interact with the global environment
;and preserve the state of the scopes
(define on_global_env
  (lambda (env func)
    (cond
      ((is_first_scope? env) (func env))
      (else (new_scope (scope_vars env) (scope_vals env) (on_global_env (pop_scope env) func)))
      )
    ))

(define get_classes
  (lambda (env)
    (car (get_first_scope env))
    ))

(define get_instances
  (lambda (env)
    (cadr (get_first_scope env))
    ))

(define _get_element_by_name
  (lambda (name elements raise? access_name_function)
    (cond
      ((null? elements) (if raise?
                           (error "[Env] No element with name :" name)
                           '()))
      ((eq? (access_name_function (car elements)) name) (car elements))
      (else (_get_element_by_name name (cdr elements) raise? access_name_function))
     )))

(define get_class_by_name
  (lambda (name env)
    (_get_element_by_name name (get_classes env) #t get_class_name)))

(define get_instance_by_ID
  (lambda (ID env)
    (_get_element_by_name ID (get_instances env) #t get_instance_ID)))

(define exist_class?
  (lambda (name env)
    (not (null? (_get_element_by_name name (get_classes env) #f get_class_name)))))

(define exist_instance?
  (lambda (ID env)
    (not (null? (_get_element_by_name ID (get_instances env) #f get_instance_ID)))))

(define add_class
  (lambda (class env)
      (on_global_env env
             (lambda (env) (global_environment (cons class (get_classes env)) (get_instances env)))
      )))

(define add_instance
  (lambda (instance env)
      (on_global_env env
             (lambda (env) (global_environment (get_classes env) (cons instance (get_instances env))))
      )))

(define _remove_element
  (lambda (name elements access_name_function)
    (cond
      ((null? elements) '())
      ((eq? name (access_name_function (car elements))) (cdr elements))
      (else (cons (car elements) (_remove_element name (cdr elements) access_name_function))))))


(define remove_class
  (lambda (class_name env)
    (on_global_env env
        (lambda (env) (global_environment (_remove_element class_name (get_classes env) get_class_name) (get_instances env)))
        )))

(define remove_instance
  (lambda (ID env)
    (on_global_env env
        (lambda (env) (global_environment (get_classes env) (_remove_element ID (get_instances env) get_instance_ID)))
        )))

(define update_class
  (lambda (old_class_name new_class env)
    (add_class new_class (remove_class old_class_name env))))

(define update_instance
  (lambda (old_instance_ID new_instance env)
    (add_instance new_instance (remove_instance old_instance_ID env))))

(define remove_first_pair
  (lambda (env)
      (new_scope (cdr (scope_vars env)) (cdr (scope_vals env)) (pop_scope env))
  ))

(define get_first_var
  (lambda (env)
    (caar env)))

(define get_first_val
  (lambda (env)
    (caadr env)))

(define null_scope?
  (lambda (env)
    (null? (car env))))

(define null_env?
  (lambda (env)
    (cond
      ((null? env) #t)
      ((null_scope? env) (null_env? (pop_scope env)))
      (else #f)
     )))

(define lookup_var
  (lambda (var env)
    (cond
      ((null_env? env) 'undefined)
      ((is_first_scope? env) 'undefined)
      ((null_scope? env) (lookup_var var (pop_scope env)))
      ((eq? var (get_first_var env)) (get_first_val env))
      (else (lookup_var var (remove_first_pair env))))))

(define var_in_env?
  (lambda (var env)
    (not (eq? (lookup_var var env) 'undefined))))

(define var_in_current_scope?
  (lambda (var env)
    (var_in_env? var (new_scope (scope_vars env) (scope_vals env) '()))))

(define add_var
  (lambda (var val env)
    (cond
      ((var_in_current_scope? var env) (error "[Env]The variable is already declared. Var: " var))
      ((is_first_scope? env) (error "[Env] The variable doesn't exist in the scope. (Var, Env): " (list var env)))
      (else (new_scope (cons var (scope_vars env)) (cons val (scope_vals env)) (pop_scope env)))
    )))

(define add_empty_var
  (lambda (var env)
    (add_var var 'uninitialized env)))

(define update_var
  (lambda (var val env)
    (cond
      ((null_env? env) (error "[Env] The variable is not present in the environment. Var:" var))
      ((null_scope? env) (new_scope (scope_vars env) (scope_vals env) (update_var var val (pop_scope env))))
      ((eq? var (get_first_var env)) (add_var var val (remove_first_pair env)))
      (else (add_var (get_first_var env) (get_first_val env) (update_var var val (remove_first_pair env)))))
    ))

(define remove_var
  (lambda (var env)
    (cond
      ((null_env? env) (error "[Env] The variable is not present in the environment. Var:" var))
      ((null_scope? env) (new_scope (scope_vars env) (scope_vals env) (remove_var var env (pop_scope env))))
      ((eq? var (get_first_var env)) (remove_first_pair env))
      (else (add_var (get_first_var env) (get_first_val env) (remove_var var (remove_first_pair env)))))))

(define var_in_static_class?
  (lambda (var class env)
    (has_static_var_in_class? var (get_class_by_name class env))))

(define var_in_class?
  (lambda (var class env)
    (has_var_in_inst? var (get_class_by_name class env))))

(define lookup_static_var_in_class
  (lambda (var class env)
    (cond
      ((var_in_static_class? var class env) (get_static_var_from_class var (get_class_by_name class env)))
      (else (error "[Env] The class has not the static variable " (list var class env))))))

(define lookup_var_in_instance
  (lambda (var class instance env)
    (cond
      ((null? class) (error "[Env] The instance has not the static variable " var))
      ((var_in_class? var class env)
                        (get_val_from_instance (get_var_position_from_class var (get_class_by_name class env))
                                               (get_instance_by_ID instance env)))
      (else (lookup_var_in_class var (get_class_parent (get_class_by_name class env)) instance env)))))

(define update_static_var_in_class
  ; class_type indicate the compile time of the class. Used to modify a filed which is "covered" by a field of the child class
  (lambda (var val class_type class env)
    (cond
      ((has_static_var_in_class? var (get_class_by_name class env))
        (on_global_env env 
                      (lambda (env) (update_class class (update_static_var_to_class var val (get_class_by_name class_type env) (get_class_by_name class env)) env))))
      (else (error "[Env] The class has not the static variable " (list var class env)))
      )))

(define update_var_in_instance
  (lambda (var val class instance env)
    (cond
      ((has_var_in_inst? var (get_class_by_name class env))
        (on_global_env env 
                      (lambda (env) (update_instance instance (update_val_to_instance val (get_var_position_from_class var (get_class_by_name class env))
                                                                                      (get_instance_by_ID instance env)) env))
                                     ))
      (else (error "[Env] The instance has not the static variable " (list var class instance env)))
      )))

(define _lookup_static_method_by_name
 (lambda (method class env raise?)
   (cond
     ((null? class) (if raise?
                        (error "[Env] The class has not the static method " method)
                        '()
                         ))
     ((has_static_method? method (get_class_by_name class env)) (get_static_method_by_name method (get_class_by_name class env)))
     (else (_lookup_static_method_by_name method (get_class_parent (get_class_by_name class env)) env raise?))
     )))

(define lookup_static_method_in_class
  (lambda (method class env)
    (_lookup_static_method_by_name method class env #t)))

(define exist_static_method_in_class?
  (lambda (method class env)
    (not (null? (_lookup_static_method_by_name method class env #f)))))

(define _lookup_method_by_name
 (lambda (method class env raise?)
   (cond
     ((null? class) (if raise?
                        (error "[Env] The class has not the method " method)
                        '()
                         ))
     ((has_method? method (get_class_by_name class env)) (get_method_by_name method (get_class_by_name class env)))
     (else (_lookup_method_by_name method (get_class_parent (get_class_by_name class env)) env raise?))
     )))

(define lookup_method_in_class
  (lambda (method class env)
    (_lookup_method_by_name method class env #t)))

(define exist_method_in_class?
  (lambda (method class env)
    (not (null? (_lookup_method_by_name method class env #f)))))

(define get_instance_class
  (lambda (instance env)
         (get_instance_type (get_instance_by_ID instance env))))

(define get_parent_class
  (lambda (class env)
    (get_class_parent (get_class_by_name class env))))

(define get_instance_parent
  (lambda (instance env)
    (get_parent_class (get_instance_type (get_instance_by_ID instance env)))))

(define var_in_instance?
  (lambda (var instance env)
    (var_in_class? var (get_instance_class instance env) env)))

;when you enter in a new scope, you can create a new instance, which is no longer
;accessible once you go out of scope. Thus you should remove it from the environment.
;But what if you set it as a field in a class?
(define garbage_collect_instances
  (lambda (env)
    ;look in the environment, the static class fields and the instance fields for instance references.
    ; (string? val) returns true if it is an instance reference. Build a list out of it and remove whatever
    ; instance is not referenced. This leaves in circular dependencies. Mark&Sweep should be the choice to go.
    ;DO NOT IMPLEMENT UNTILL YOU ACTUALLY NEED IT (for example if there is a big loop in the code this could be needed)
    (error "Not implemented")))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;VARIABLE DECLARATION FUNCTIONS

(define var_declaration?
  (lambda (stmt)
    (eq? (get_operation_name stmt) 'var)))

(define state_var_decl
  (lambda (stmt class instance env return continue break throw)
    (cond
      ((= (length stmt) 2) (add_empty_var (get_first_argument stmt) env))
      ((= (length stmt) 3) (add_var (get_first_argument stmt) (value_eval (get_second_argument stmt) class instance env return continue break throw)
                                    (state_eval (get_second_argument stmt) class instance env return continue break throw)))
      (else (error "[Variable Declaration] Bad inpunt. Input:"stmt)))))

(define value_var_decl
  (lambda (stmt class instance env return continue break throw)
    (error "[Variable Declaration] Variable Declaration statement has no value.")))


;VARIABLE ASSIGNMENT FUNCTIONS
(define var_assignment?
  (lambda (stmt)
    (eq? (get_operation_name stmt) '=)))

(define state_var_assign
  (lambda (stmt class instance env return continue break throw)
    (if (not (stmt? (get_first_argument stmt)))
        (cond
              ((var_in_env? (get_first_argument stmt) env)
                      (update_var (get_first_argument stmt) (value_eval (get_second_argument stmt) class instance env return continue break throw) (state_eval (get_second_argument stmt) class instance env return continue break throw)))
              (else (state_var_assign (list (get_operation_name stmt) (list 'dot 'this (get_first_argument stmt)) (get_second_argument stmt))
                                  class instance env return continue break throw))
        )
        (update_dot_variable (get_first_argument (get_first_argument stmt)) (get_second_argument (get_first_argument stmt)) (get_second_argument stmt)
                             class instance env return continue break throw))))
        

(define value_var_assign
  (lambda (stmt class instance env return continue break throw)
     (value_eval (get_second_argument stmt) class instance (state_eval (get_second_argument stmt) class instance env return continue break throw) return continue break throw)
  ))

;VARIABLE ACCESS

(define search_var
  (lambda (stmt class instance env ignore_class ignore_instance ignore_env)
    (cond
      ((not ignore_env) (if (var_in_env? stmt env)
                              (lookup_var stmt env)
                              (search_var stmt class instance env #f (null? instance) #t)))
      ((not ignore_instance) (if (var_in_class? stmt class env)
                                   (lookup_var_in_instance stmt class instance env)
                                   (search_var stmt class instance env #f #t #t)))
      ((not ignore_class) (if (var_in_static_class? stmt class env)
                                (lookup_static_var_in_class stmt class env)
                                (search_var stmt class instance env #t #t #t)))
      (else 'undeclared))))

;the return_env flag allows me to use the same function both in state_ and value_
(define _var_dispatch
  (lambda (var_value stmt env return_env)
    (cond
      ((eq? var_value 'undefined) (error "[VARIABLE ACCESS] The variable is not defined. Var :" stmt))
      ((eq? var_value 'undeclared) (error "[VARIABLE ACCESS] The variable is not declared. Var :" stmt))
      (else (if return_env
                env
                var_value)))))

(define state_var_access
  (lambda (stmt class instance env return continue break throw)
    (_var_dispatch (search_var stmt class instance env #f (null? instance) #f) stmt env #t)))

(define value_var_access
  (lambda (stmt class instance env return continue break throw)
    (_var_dispatch (search_var stmt class instance env #f (null? instance) #f) stmt env #f)
    ))
  

;RETURN FUNCTIONS
(define return?
  (lambda (stmt)
    (eq? (get_operation_name stmt) 'return)))

(define state_return
  (lambda (stmt class instance env return continue break throw)
    (return (make_return (value_eval (get_first_argument stmt) class instance env return continue break throw) (state_eval (get_first_argument stmt) class instance env return continue break throw)))))

(define value_return
  (lambda (stmt class instance env return continue break throw)
    (error "[RETURN] Return statement has no value.")))

;IF STATEMENT FUNCTIONS
(define if_stmt?
  (lambda (stmt)
    (eq? (get_operation_name stmt) 'if)))

(define state_if
  (lambda (stmt class instance env return continue break throw)
    (cond
      ((eq? (length stmt) 3)
       (if (eq? (value_eval (get_first_argument stmt) class instance env return continue break throw) 'true)
           (state_eval (get_second_argument stmt) class instance (state_eval (get_first_argument stmt) class instance env return continue break throw) return continue break throw)
           env
      ))
      ((eq? (length stmt) 4)
       (if (eq? (value_eval (get_first_argument stmt) class instance env return continue break throw) 'true)
           (state_eval (get_second_argument stmt) class instance (state_eval (get_first_argument stmt) class instance env return continue break throw) return continue break throw)
           (state_eval (get_third_argument stmt) class instance (state_eval (get_first_argument stmt) class instance env return continue break throw) return continue break throw)
      ))
      (else (error "[IF] Bad input. Input:" stmt)))))

(define value_if
  (lambda (stmt class instance env return continue break throw)
    (error "[IF] If statement has no value.")))

;OPERATION FUNCTIONS
(define math_op?
  (lambda (stmt)
    (cond
      ((eq? (get_operation_name stmt) '+) #t)
      ((eq? (get_operation_name stmt) '-) #t)
      ((eq? (get_operation_name stmt) '*) #t)
      ((eq? (get_operation_name stmt) '/) #t)
      ((eq? (get_operation_name stmt) '%) #t)
      (else #f))))

(define operator_to_function
  (lambda (oper)
    (cond
      ((eq? oper '+) +)
      ((eq? oper '-) -)
      ((eq? oper '*) *)
      ((eq? oper '/) quotient)
      ((eq? oper '%) modulo)
      (else (error "[Operation Function] No know operation for " oper))
      )))

(define state_math_op
  (lambda (stmt class instance env return continue break throw)
    (if (eq? (length stmt) 2)
        (state_eval (get_first_argument stmt) class instance env return continue break throw)
        (state_eval (get_second_argument stmt) class instance (state_eval (get_first_argument stmt) class instance env return continue break throw) return continue break throw)
    )))

(define value_math_op
  (lambda (stmt class instance env return continue break throw)
    (if (eq? (length stmt) 2)
        
        ((operator_to_function (get_operation_name stmt)) (value_eval (get_first_argument stmt) class instance env return continue break throw)) ;the only unary operator is -
        
        ((operator_to_function (get_operation_name stmt))
         (value_eval (get_first_argument stmt) class instance env return continue break throw)
         (value_eval (get_second_argument stmt) class instance (state_eval (get_first_argument stmt) class instance env return continue break throw) return continue break throw)
         )
     )))

;COMPARISON FUNCTIONS
(define comparison?
  (lambda (stmt)
    (cond
      ((eq? (get_operation_name stmt) '==) #t)
      ((eq? (get_operation_name stmt) '!=) #t)
      ((eq? (get_operation_name stmt) '<) #t)
      ((eq? (get_operation_name stmt) '>) #t)
      ((eq? (get_operation_name stmt) '<=) #t)
      ((eq? (get_operation_name stmt) '>=) #t)
      (else #f))))

(define comparison_to_function
  (lambda (oper)
    (cond
      ((eq? oper '==) =)
      ((eq? oper '!=) (lambda (a b) (not (= a b))))
      ((eq? oper '<) <)
      ((eq? oper '>) >)
      ((eq? oper '<=) <=)
      ((eq? oper '>=) >=)
      (else (error "[Comparison Function] No know comparison for " oper))
      )))

(define state_comparison
  (lambda (stmt class instance env return continue break throw)
    (state_eval (get_second_argument stmt) class instance (state_eval (get_first_argument stmt) class instance env return continue break throw) return continue break throw)))

(define value_comparison
  (lambda (stmt class instance env return continue break throw)
    (convert_from_bool ((comparison_to_function (get_operation_name stmt))
     (value_eval (get_first_argument stmt) class instance env return continue break throw)
     (value_eval (get_second_argument stmt) class instance (state_eval (get_first_argument stmt) class instance env return continue break throw) return continue break throw)
     ))))

;BOOLEAN FUNCTIONS
(define boolean_func?
  (lambda (stmt)
    (cond
      ((eq? (get_operation_name stmt) '&&) #t)
      ((eq? (get_operation_name stmt) '||) #t)
      ((eq? (get_operation_name stmt) '!) #t)
      (else #f))))

(define boolean_to_function
  (lambda (oper)
    (cond
      ((eq? oper '&&) (lambda (a b) (and a b)))
      ((eq? oper '||) (lambda (a b) (or a b)))
      ((eq? oper '!)  (lambda (a) (not a)))
      (else (error "[Boolean Function] No know boolean operation for " oper))
      )))
        
(define state_boolean
  (lambda (stmt class instance env return continue break throw)
    (if (eq? (length stmt) 2)
        (state_eval (get_first_argument stmt) class instance env return continue break throw)
        (state_eval (get_second_argument stmt) class instance (state_eval (get_first_argument stmt) class instance env return continue break throw) return continue break throw)
    )))

(define value_boolean
  (lambda (stmt class instance env return continue break throw)
    (if (eq? (length stmt) 2)
        (convert_from_bool ((boolean_to_function (get_operation_name stmt))
                            (convert_to_bool (value_eval (get_first_argument stmt) class instance env return continue break throw)))
        )
        (convert_from_bool ((boolean_to_function (get_operation_name stmt))
                            (convert_to_bool (value_eval (get_first_argument stmt) class instance env return continue break throw))
                            (convert_to_bool (value_eval (get_second_argument stmt) class instance (state_eval (get_first_argument stmt) class instance env return continue break throw) return continue break throw)))
        )
     )))

;BEGIN FUNCTIONS
(define begin_func?
  (lambda (stmt)
    (eq? (get_operation_name stmt) 'begin)))

(define on_begin_scope_exit
  (lambda (env)
    (pop_scope env)))

(define on_begin_scope_entrance
  (lambda (env)
    (new_empty_scope env)))

(define state_begin
  (lambda (stmt class instance env return continue break throw)
    (on_begin_scope_exit (get_env_from_return
                           (state_eval_stmt_tree
                            (get_all_arguments stmt)
                            class
                            instance 
                            (make_return (no_value) (on_begin_scope_entrance env))
                            return
                            (lambda (cont) (continue (on_begin_scope_exit cont)))
                            (lambda (bre) (break (on_begin_scope_exit  bre)))
                            (lambda (thr) (throw (make_return (get_val_from_return thr)
                                                              (on_begin_scope_exit (get_env_from_return thr)))))
                           )
                          ))
   ))

(define value_begin
  (lambda (stmt class instance env return continue break throw)
    (error "[Begin Function] A scope has no value. Statement: " stmt)))

;BREAK FUNCTIONS
(define break_func?
  (lambda (stmt)
    (eq? (get_operation_name stmt) 'break)))

(define state_break
  (lambda (stmt class instance env return continue break throw)
    (break env)))

(define value_break
  (lambda (stmt class instance env return continue break throw)
    (error "[Break Function] Break has no value.")))

;CONTINUE FUNCTIONS
(define continue_func?
  (lambda (stmt)
    (eq? (get_operation_name stmt) 'continue)))

(define state_continue
  (lambda (stmt class instance env return continue break throw)
    (continue env)))

(define value_continue
  (lambda (stmt class instance env return continue break throw)
    (error "[Continue Function] Continue has no value.")))

;WHILE FUNCTIONS
(define while_func?
  (lambda (stmt)
    (eq? (get_operation_name stmt) 'while)))

(define while_loop
  (lambda (cond stmt class instance env return continue break throw)
    ;(begin (display env) (display (newline))
    (if (convert_to_bool (value_eval cond class instance env return continue break throw))
        (while_loop cond stmt class instance (state_eval stmt class instance ( state_eval cond class instance env return continue break throw) return continue break throw)
                    return continue break throw)
        (break (state_eval cond class instance env return continue break throw))
    ;)
 )))

(define continue_loop
  (lambda (cond stmt class instance env return continue break throw)
    (continue_loop cond stmt class instance (call/cc (lambda (continue)
          (while_loop cond stmt class instance env return continue break throw)
         )) return continue break throw)
    ))

(define state_while
  (lambda (stmt class instance env return continue break throw)
      (call/cc (lambda (break-while)
         (continue_loop (get_first_argument stmt) (get_second_argument stmt) class instance env return continue break-while throw)
      ))
   ))

(define value_while
  (lambda (stmt class instance env return continue break throw)
    (error "[While Function] While has no value.")))

;FUNCTION CALL FUNCTIONS

(define func_call?
  (lambda (stmt)
    (eq? (get_operation_name stmt) 'funcall)))

(define get_funcall_param_list
  (lambda (stmt)
    (cddr stmt)))

(define first_param
  (lambda (params)
    (cond
      ((eq? (car params) '&) (cadr params))
      (else (car params)))))

(define first_param_call_by_reference
  (lambda (params)
    (if (eq? (car params) '&)
        #t
        #f)
   ))

(define next_param
  (lambda (params)
    (cond
      ((eq? (car params) '&) (cddr params))
      (else (cdr params)))))

(define apply_param_side_effects_to_env
  (lambda (class instance env vals return continue break throw)
    (cond
      ((null? vals) env)
      (else (apply_param_side_effects_to_env class instance (state_eval (car vals) class instance env return continue break throw) (cdr vals) return continue break throw))
      )))

(define map_parameters
  (lambda (param vals class instance env orig_env return continue break throw)
    (cond  
      ((and (null? param) (not (null? vals))) (error "[Function Call] The number of parameters doesn't match with the function declaration."))
      ((null? param) env)
      (else (add_var (first_param param)
                     (value_eval (car vals) class instance orig_env return continue break throw)
                     (map_parameters (next_param param) (cdr vals) class instance env (state_eval (car vals) class instance orig_env return continue break throw) return continue break throw)))
      )))

(define build_function_env
  (lambda (stmt func class instance env return continue break throw)
    (map_parameters
           (get_method_param func)
           (get_funcall_param_list stmt)
           class
           instance
           (new_empty_scope (get_first_scope (apply_param_side_effects_to_env class instance env (get_funcall_param_list stmt) return continue break throw)))
           env
           return continue break throw
     )))

(define func_call
  (lambda (stmt func class instance env return continue break throw function_throw)
    (call/cc (lambda (return1)
       (state_eval_stmt_tree (get_method_code (call_closure func))
                             (get_method_class (call_closure func))
                             (call_instance_context func)
                             (make_return (no_value) (build_function_env stmt (call_closure func) class instance env return continue break throw))
                             return1
                             (error_function "[Eval] Continue is not valid in this context.")
                             (error_function "[Eval] Break is not valid in this context.")
                             function_throw
       )))
    ))

(define replace_first_layer
  (lambda (env func_env_first_scope)
   (on_global_env env (lambda (env) func_env_first_scope))))

(define merge_envs
    (lambda (env func_env)
      ; we can try to remove the unused instances here
      (replace_first_layer env (get_first_scope func_env))))

(define call_inf (lambda (is_static instance closure) (list is_static instance closure)))
(define is_static_call? (lambda (call_inf) (car call_inf)))
(define call_instance_context ( lambda (call_inf) (cadr call_inf)))
(define call_closure (lambda (call_inf) (caddr call_inf)))

(define get_function_closure
  (lambda (stmt class instance env return continue break throw)
    (cond
      ((and (stmt? (get_first_argument stmt)) (dot? (get_first_argument stmt)))
                         (dot_function_access (get_first_argument stmt) class instance env return continue break throw))
      ((and (not (null? instance)) (exist_method_in_class? (get_first_argument stmt) (get_instance_class instance env) env))
                         (call_inf #f instance (lookup_method_in_class (get_first_argument stmt) (get_instance_class instance env)  env)))
      ((exist_static_method_in_class? (get_first_argument stmt) class env)
                         (call_inf #t '() (lookup_static_method_in_class (get_first_argument stmt) class  env)))
      (else (error "No idea what to do"))
    )))

(define state_func_call
  (lambda (stmt class instance env return continue break throw)
    (let ((function_closure (get_function_closure stmt class instance env return continue break throw))
          (side_effect_env (apply_param_side_effects_to_env class instance env (get_funcall_param_list stmt) return continue break throw)))
         (merge_envs
	            side_effect_env
	            (get_env_from_return
                     (func_call stmt function_closure class instance env return continue break throw
                          ;when we throw inside a function we want to give to the catch/finally not the function scope
                          ;but the original scope, so recontruct it here.
                          (lambda (thr) (throw (make_return (get_val_from_return thr)
                                                            (merge_envs side_effect_env (get_env_from_return thr)))))))
         )
     )))

(define value_func_call
  (lambda (stmt class instance env return continue break throw)
    (let ((function_closure (get_function_closure stmt class instance env return continue break throw))
          (side_effect_env (apply_param_side_effects_to_env class instance env (get_funcall_param_list stmt) return continue break throw)))
      ; no need to reconstruct any environment as we only care about the value
      (get_val_from_return (func_call stmt function_closure class instance env return continue break throw
                                      (lambda (thr) (throw (make_return (get_val_from_return thr)
                                                            (merge_envs side_effect_env (get_env_from_return thr)))))))
    )))

;TRY FUNCTIONS

(define try?
  (lambda (stmt)
    (eq? (get_operation_name stmt) 'try)))

(define get_try
  (lambda (stmt) (list 'try (get_first_argument stmt))))

; the try code is exectured with a state_begin call. Since it ignores the first parameter
; expecting it to be a "begin", we put a placeholder not to lose any executable code.
(define get_try_code
  (lambda (try) (cons 'placeholder (get_first_argument try))))

(define get_catch
  (lambda (stmt)
    (get_second_argument stmt)))

(define get_catch_code
  (lambda (catch) (get_second_argument catch)))

(define get_catch_throw_var
  (lambda (catch) (car (get_first_argument catch))))

(define get_finally
  (lambda (stmt)
      (get_third_argument stmt)))

;see get_try_code
(define get_finally_code
  (lambda (finally) (cons 'placeholder (get_first_argument finally))))

(define prepare_catch_env
  (lambda (catch throw)
    (add_var (get_catch_throw_var catch) (get_val_from_return throw) (on_begin_scope_entrance (get_env_from_return throw)))))

(define execute_catch
  (lambda (try catch class instance env return continue break throw)
    (if (null? catch)
        (state_begin (get_try_code try) class instance env return continue break throw)
        (call/cc (lambda (throw1) (state_begin (get_try_code try) class instance env return continue break
                                   ; you get a throw as parameter: (val, env), but you need tp call throw1 with an env
                                   (lambda (thr_ret) (throw1 (on_begin_scope_exit (get_env_from_return
           ;throw function. Starts ^ there
           (state_eval_stmt_tree (get_catch_code catch) class instance (make_return (no_value) (prepare_catch_env catch thr_ret))
                                 ;we pushed a new scope in prepare_catch_env, so it's our duty to pop it
                                 return
                                 (lambda (cont) (continue (on_begin_scope_exit cont)))
                                 (lambda (bre) (break (on_begin_scope_exit bre)))
                                 (lambda (thr) (throw (make_return (get_val_from_return thr) (on_begin_scope_exit (get_env_from_return thr)))))
                                 ))))); <-- Ends here
       ))))))

(define execute_finally
  (lambda (try catch finally class instance env return continue break throw)
      (if (null? finally)
          (execute_catch try catch class instance env return continue break throw)
          (state_begin (get_finally_code finally) class instance
                       (execute_catch try catch class instance env
                               (lambda (ret) (return (make_return
                                                         (get_val_from_return ret)
                                                         (state_begin (get_finally_code finally) class instance (get_env_from_return ret) return continue break throw))))
                               (lambda (cont) (continue
                                                (state_begin (get_finally_code finally) class instance cont return continue break throw)))
                               (lambda (bre) (break
                                                (state_begin (get_finally_code finally) class instance bre return continue break throw)))
                               (lambda (thr) (throw (make_return
                                                         (get_val_from_return thr)
                                                         (state_begin (get_finally_code finally) class instance (get_env_from_return thr) return continue break throw)))))
                       return continue break throw)
      )))

(define state_try
  (lambda (stmt class instance env return continue break throw)
    (execute_finally (get_try stmt) (get_catch stmt) (get_finally stmt) class instance env return continue break throw)
    ))
    

(define value_try
  (lambda (stmt class instance env return continue break throw)
    (error "[TRY STATEMENT] Try statement has no value.")))


;THROW FUNCTIONS
(define throw?
  (lambda (stmt)
    (eq? (get_operation_name stmt) 'throw)))

(define state_throw
  (lambda (stmt class instance env return continue break throw)
    (throw (make_return (value_eval (get_first_argument stmt) class instance env return continue break throw) (state_eval (get_first_argument stmt) class instance env return continue break throw)))))

(define value_throw
  (lambda (stmt class instance env return continue break throw)
    (error "[THROW] Throw statement has no value.")))

;DOT
(define dot?
  (lambda (stmt)
    (eq? (get_operation_name stmt) 'dot)))

(define this?
  (lambda (atom)
    (eq? atom 'this)))

(define super?
  (lambda (atom)
    (eq? atom 'super)))

(define instance?
  (lambda (stmt)
    (string? stmt)))

(define update_dot_intance
  (lambda (left_dot var val_expr class instance env return continue break throw)
    (let* (
          (left_value (value_eval left_dot class instance env return continue break throw))
          (left_state (state_eval left_dot class instance env return continue break throw))
          (val_value (value_eval val_expr class instance left_state return continue break throw))
          (val_state (state_eval val_expr class instance left_state return continue break throw))
          ;(environment (display val_state))
          )
      (cond
        ; the dot resolves to an instance and the instance has the field
        ((and (instance? left_value) (var_in_instance? var left_value left_state))
              (update_var_in_instance var val_value
                                      (get_instance_class left_value val_state)
                                      left_value
                                      val_state))
        ; the dot resolves to an instance but it refers to a static filed.
        ; should this be supported? It is bad practice to access fileds like that..
        ((instance? left_value) (update_static_var_in_class var val_value
                                                           (get_instance_class left_value val_state)
                                                           (get_instance_class left_value val_state)
                                                           val_state))
        ; the dot resolves to a class name
        ((exist_class? left_value left_state) (update_static_var_in_class var val_value left_value left_value val_state))
        (else (error "[Assignment Functions] No way to update " (list left_dot var val_expr class instance env)))
       ))))

(define update_dot_this/super
  (lambda (var val class instance env)
    (cond
      ((null? instance) (update_static_var_in_class var val class class env ))
      ((var_in_class? var class env)
                        (update_var_in_instance var val class instance env))
      (else (update_static_var_in_class var val class env ))
      )))

(define update_dot_variable
  (lambda (left_dot right_dot val_expr class instance env return continue break throw)
    (let ((val (value_eval val_expr class instance env return continue break throw))
          (new_env (state_eval val_expr class instance env return continue break throw)))
     (cond
        ((this? left_dot) (update_dot_this/super right_dot val class instance new_env))
        ((super? left_dot) (update_dot_this/super right_dot val (get_parent_class class env) instance new_env))
        (else (update_dot_intance left_dot right_dot val_expr class instance env return continue break throw))
       ))))

(define dot_variable_assignment_static_class
  (lambda (dot_stmt class pre_eval_env return continue break throw)
    (cond
      ((this? (get_first_argument dot_stmt))
                   class)
      ((super? (get_first_argument dot_stmt))
                   (get_parent_class class env))
      (else (value_eval (get_first_argument dot_stmt) class '() pre_eval_env return continue break throw))
     )))

(define method_dot_instance
  (lambda (stmt class instance env return continue break throw)
    (let ((val (get_second_argument stmt))
          (evald_first_arg (value_eval (get_first_argument stmt) class instance env return continue break throw))
          (evaluated_state (state_eval (get_first_argument stmt) class instance env return continue break throw)))
      (cond
        ((and (instance? evald_first_arg) (exist_method_in_class? val (get_instance_class evald_first_arg env) env))
              (call_inf #f evald_first_arg (lookup_method_in_class val (get_instance_class evald_first_arg env) env)))
        (else (call_inf #t '() (lookup_static_method_in_class val evald_first_arg evaluated_state)))
       ))))

(define method_dot_this
  (lambda (stmt class instance env)
    (cond
      ((null? instance) (call_inf #t '() (lookup_static_method_in_class (get_second_argument stmt) class env )))
      ((exist_method_in_class? (get_second_argument stmt) (get_instance_class instance env) env)
                        (call_inf #f instance (lookup_method_in_class (get_second_argument stmt) (get_instance_class instance env) env)))
      ; you need to access the function as static, so just recall this method with null instance
      (else (method_dot_this/super stmt class '() env))
      )))

(define method_dot_super
  (lambda (stmt class instance env)
    (cond
      ((null? instance) (call_inf #t '() (lookup_static_method_in_class (get_second_argument stmt) (get_parent_class class env) env )))
      ((exist_method_in_class? (get_second_argument stmt) (get_parent_class class env) env)
                        (call_inf #f instance (lookup_method_in_class (get_second_argument stmt) (get_parent_class class env) env)))
      ; you need to access the function as static, so just recall this method with null instance
      (else (method_dot_this/super stmt class '() env))
      )))

(define dot_function_access
  (lambda (stmt class instance env return continue break throw)
    (cond
        ((this? (get_first_argument stmt)) (method_dot_this stmt class instance env))
        ((super? (get_first_argument stmt)) (method_dot_super stmt class instance env))
        (else (method_dot_instance stmt class instance env return continue break throw))
       )))

(define state_dot
  (lambda (stmt class instance env return continue break throw)
    (cond
      ;if it isn't a statement then it can not change the environment! (for example ClassName.x)
      ((this? (get_first_argument stmt)) env)
      ((super? (get_first_argument stmt)) env)
      ((not (stmt? stmt)) env)
      (else (state_eval (get_first_argument stmt) class instance env return continue break throw))
    )))

(define value_dot_instance
  (lambda (stmt class instance env return continue break throw)
    (let ((val (get_second_argument stmt))
          (evald_first_arg (value_eval (get_first_argument stmt) class instance env return continue break throw))
          (evaluated_state (state_eval (get_first_argument stmt) class instance env return continue break throw)))
      (cond
        ((and (instance? evald_first_arg) (var_in_instance? val evald_first_arg env))
              (lookup_var_in_instance val (get_instance_class evald_first_arg env) evald_first_arg env))
        (else (lookup_static_var_in_class val evald_first_arg evaluated_state))
       ))))

(define value_dot_this/super
  (lambda (stmt class instance env)
    (cond
      ((null? instance) (lookup_static_var_in_class (get_second_argument stmt) class env ))
      ((var_in_class? (get_second_argument stmt) class env)
                        (lookup_var_in_instance (get_second_argument stmt) class instance env))
      (else (lookup_static_var_in_class (get_second_argument stmt) class env ))
      )))
      
(define value_dot
  (lambda (stmt class instance env return continue break throw)
      (cond
        ((this? (get_first_argument stmt)) (value_dot_this/super stmt class instance env))
        ((super? (get_first_argument stmt)) (value_dot_this/super stmt (get_parent_class class env) instance env))
        (else (value_dot_instance stmt class instance env return continue break throw))
       )))

;NEW FUNCTIONS
(define new?
  (lambda (stmt)
    (eq? (get_operation_name stmt) 'new)))

(define update_instance_value_by_position
  (lambda (position val class instance env)
        (on_global_env env 
          (lambda (env) (update_instance instance
                            (update_val_to_instance val position (get_instance_by_ID instance env)) env))
   )))

(define initialize_values
  (lambda (position vals class ID env return continue break throw)
    (cond
      ((null? vals) env)
      (else (initialize_values (- position 1) (cdr vals) class ID
             (if (eq? (car vals) 'undefined)
                 env
                 (update_instance_value_by_position position
                                     (value_eval (car vals) class ID env return continue break throw)
                                     class ID
                                     (state_eval (car vals) class ID env return continue break throw))
             )
             return continue break throw))
    )))

(define state_new
  (lambda (stmt class instance env return continue break throw)
    (let ((ID (pull_ID))
          (new_class (get_first_argument stmt)))
          (initialize_values (- (length (get_class_vars (get_class_by_name new_class env))) 1)
                             (get_class_vals_default (get_class_by_name new_class env))
                             new_class ID
                             (add_instance (make_instance ID new_class (build-list (length (get_class_vars (get_class_by_name new_class env))) (lambda (x) 'undefined))) env)
                             return continue break throw))))

(define value_new
  (lambda (stmt class instance env return continue break throw)
    (push_ID (unique_id))))


;EVALUATION FUNCTIONS

(define stmt?
  (lambda (stmt)
    (list? stmt)))

(define state_eval
  (lambda (stmt class instance env return continue break throw)
    (cond
      ;first check if null
      ((null? stmt) env)
      ;then check if number or variable
      ((number? stmt) env)     
      ((eq? stmt 'true) env)
      ((eq? stmt 'false) env)
      ((exist_class? stmt env) env)
      ((instance? stmt) env)
      ((not (stmt? stmt)) (state_var_access stmt class instance env
                                               (error_function "[Variable Access] You can not return inside a variable access")
                                               (error_function "[Variable Access] You can not continue inside a variable access")
                                               (error_function "[Variable Access] You can not break inside a variable access")
                                               (error_function "[Variable Access] You can not throw inside a variable access")))
      ;then check what kind of statement it is
      ((var_declaration? stmt) (state_var_decl stmt class instance env
                                               (error_function "[Variable Declaration] You can not return inside a variable declaration")
                                               (error_function "[Variable Declaration] You can not continue inside a variable declaration")
                                               (error_function "[Variable Declaration] You can not break inside a variable declaration")
                                               throw))
      ((var_assignment? stmt) (state_var_assign stmt class instance env
                                                (error_function "[Variable Assignment] You can not return inside a variable assignment")
                                                (error_function "[Variable Assignment] You can not continue inside a variable assignment")
                                                (error_function "[Variable Assignment] You can not break inside a variable assignment")
                                                throw))
      ((return? stmt) (state_return stmt class instance env
                                    return
                                    (error_function "[Return Function] You can not continue inside a return function")
                                    (error_function "[Return Function] You can not break inside a return function")
                                    throw))
      ((if_stmt? stmt) (state_if stmt class instance env return continue break throw))
      ((math_op? stmt) (state_math_op stmt class instance env
                                      (error_function "[Operation Function] You can not return inside a math operation")
                                      (error_function "[Operation Function] You can not continue inside a math operation")
                                      (error_function "[Operation Function] You can not break inside a math operation")
                                      throw))
      ((comparison? stmt) (state_comparison stmt class instance env
                                            (error_function "[Comparison Function] You can not return inside a comparison function")
                                            (error_function "[Comparison Function] You can not continue inside a comparison function")
                                            (error_function "[Comparison Function] You can not break inside a comparison function")
                                            throw))
      ((boolean_func? stmt) (state_boolean stmt class instance env
                                           (error_function "[Boolean Function] You can not return inside a boolean function")
                                           (error_function "[Boolean Function] You can not continue inside a boolean function")
                                           (error_function "[Boolean Function] You can not break inside a boolean function")
                                           throw))
      ;begin push a new scope, but if you break or continue, call/cc cancels the stack and then the scope will not be popped. Need to wrap the 2 continuation functions
      ;now it's done in the state_begin code
      ((begin_func? stmt) (state_begin stmt class instance env return continue break throw))
      ((break_func? stmt) (state_break stmt class instance env return continue break throw))
      ((continue_func? stmt) (state_continue stmt class instance env return continue break throw))
      ((while_func? stmt) (state_while stmt class instance env return continue break throw))
      ((func_call? stmt) (state_func_call stmt class instance env return continue break throw))
      ((try? stmt) (state_try stmt class instance env return continue break throw))
      ((throw? stmt) (state_throw stmt class instance env return continue break throw))
      ((dot? stmt) (state_dot stmt class instance env return continue break throw))
      ((new? stmt) (state_new stmt class instance env return continue break throw))
      
      (else (error "[Eval] No state function can evaluate" stmt)))))

(define value_eval
  (lambda (stmt class instance env return continue break throw)
    (cond
      ((null? stmt) (no_value))
      ((number? stmt) stmt)
      ((eq? stmt 'true) 'true)
      ((eq? stmt 'false) 'false)
      ((exist_class? stmt env) stmt)
      ((instance? stmt) stmt)
      ((not (stmt? stmt)) (value_var_access stmt class instance env return continue break throw))
      ((var_declaration? stmt) (value_var_decl stmt class instance env return continue break throw))
      ((var_assignment? stmt) (value_var_assign stmt class instance env return continue break throw))
      ((return? stmt) (value_return stmt class instance env return continue brea throwk))
      ((if_stmt? stmt) (value_if stmt class instance env return continue break throw))
      ((math_op? stmt) (value_math_op stmt class instance env return continue break throw))
      ((comparison? stmt) (value_comparison stmt class instance env return continue break throw))
      ((boolean_func? stmt) (value_boolean stmt class instance env return continue break throw))
      ((begin_func? stmt) (value_begin stmt class instance env return continue break throw))
      ((break_func? stmt) (value_break stmt class instance env return continue break throw))
      ((continue_func? stmt) (value_continue stmt class instance env return continue break throw))
      ((while_func? stmt) (value_while stmt class instance env return continue break throw))
      ((func_call? stmt) (value_func_call stmt class instance env return continue break throw))
      ((try? stmt) (value_try stmt class instance env return continue break throw))
      ((throw? stmt) (value_throw stmt class instance env return continue break throw))
      ((dot? stmt) (value_dot stmt class instance env return continue break throw))
      ((new? stmt) (value_new stmt class instance env return continue break throw))
      
      (else (error "[Eval] No value function can evaluate" stmt)))))

(define state_eval_stmt_tree
  (lambda (stmt_tree class instance ret return continue break throw)
    (cond
      ;REMEMBER: it returns a RETURN, not an environment
      ((null? stmt_tree) ret)
      (else 
       ;(begin (display (list (car stmt_tree) '!!!! class '!!!! instance '!!!! (get_env_from_return ret))) (newline)
       (state_eval_stmt_tree (cdr stmt_tree) class instance
                             (make_return (no_value)
                                 (state_eval (car stmt_tree) class instance (get_env_from_return ret) return continue break throw))
                                 return continue break throw)
       ;)
       ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;, ENVIRONMENT BUILDING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;STATIC BODY PARSING

(define static-method?
  (lambda (stmt)
    (eq? (car stmt) 'static-function)))

(define method?
  (lambda (stmt)
    (eq? (car stmt) 'function)))

(define static-var?
  (lambda (stmt)
    (eq? (car stmt) 'static-var)))

(define var?
  (lambda (stmt)
    (eq? (car stmt) 'var)))

(define parse_static_func
  (lambda (stmt class)
    (add_static_method_to_class (make_method (get_first_argument stmt) (get_second_argument stmt) (get_third_argument stmt) (get_class_name class)) class)))

(define parse_func
  (lambda (stmt class)
    (add_method_to_class (make_method (get_first_argument stmt) (get_second_argument stmt) (get_third_argument stmt) (get_class_name class)) class)))

(define parse_static_var
  (lambda (stmt class env)
    (if (= (length stmt) 2)
        (add_static_var_to_class (get_first_argument stmt) 'undefined class)
        (add_static_var_to_class (get_first_argument stmt)
                                 (value_eval (get_second_argument stmt) (get_class_name class) '() (add_class class env)
                                             (error_function "No return in class construction")
                                             (error_function "No continue in class construction")
                                             (error_function "No break in class construction")
                                             (error_function "No throw in class construction"))
                                 class))))

(define static_var_initialization_side_effects
  (lambda (stmt class env)
    (if (= (length stmt) 2)
        env
        (state_eval (get_second_argument stmt) (get_class_name class) '() (add_class class env)
                                             (error_function "No return in class construction")
                                             (error_function "No continue in class construction")
                                             (error_function "No break in class construction")
                                             (error_function "No throw in class construction")))))

(define parse_var
  (lambda (stmt class env)
    (if (= (length stmt) 2)
        (add_var_to_class (get_first_argument stmt) 'undefined class)
        ; store the initialization code to be executed on creation
        (add_var_to_class (get_first_argument stmt) (get_second_argument stmt) class))))

(define parse_class_body
  (lambda (body class env)
    (cond
      ((null? body) (add_class class env))
      ((static-method? (car body)) (parse_class_body (cdr body) (parse_static_func (car body) class) env))
      ((method? (car body))        (parse_class_body (cdr body) (parse_func (car body) class) env))
      ((static-var? (car body))    (parse_class_body (cdr body) (parse_static_var (car body) class env) (static_var_initialization_side_effects (car body) class env)))
      ((var? (car body))           (parse_class_body (cdr body) (parse_var (car body) class env) env))
      (else (error "[Class Declaration] No know statement : " (car body))))))
      ;(else (parse_class_body (cdr body) func_list)))))

(define initialize_extending_class
  (lambda (class parent_name env)
    (append_vars_to_class (get_class_vars (get_class_by_name parent_name env))
                          (get_class_vals_default (get_class_by_name parent_name env))
                          (append_static_vars_to_class (get_class_static_vars (get_class_by_name parent_name env))
                                                       (get_class_static_vals (get_class_by_name parent_name env))
                                                       class)
     )))

;;CLASS PARSING

(define is_class?
  (lambda (stmt)
    (eq? (car stmt) 'class)))

(define get_class_stmt_name
  (lambda (stmt)
    (cadr stmt)))

(define get_class_stmt_parent
  (lambda (stmt)
    (if (null? (caddr stmt))
        '()
        (cadr (caddr stmt)))))

(define get_class_stmt_body
  (lambda (stmt)
    (cadddr stmt)))

(define parse_class
  (lambda (class_stmt env)
    (if (null? (get_class_stmt_parent class_stmt))
        (parse_class_body (get_class_stmt_body class_stmt) (new_class (get_class_stmt_name class_stmt)) env)
        (parse_class_body (get_class_stmt_body class_stmt)
                          (initialize_extending_class (new_class_extends (get_class_stmt_name class_stmt) (get_class_stmt_parent class_stmt)) (get_class_stmt_parent class_stmt) env)
                          env)
        )))

(define parse_body
  (lambda (file_body env)
    (cond
      ((null? file_body) env)
      ((is_class? (car file_body)) (parse_body (cdr file_body) (parse_class (car file_body) env)))
      (else (error "[Parsing] Expected a class declaration. Obtained : " file_body)))))

(define _interpret
  (lambda (class_name global_env)
       (value_func_call '(funcall main) class_name '() global_env
                         (error_function "[Eval] Return is not valid in this context.")
                         (error_function "[Eval] Continue is not valid in this context.")
                         (error_function "[Eval] Break is not valid in this context.")
                         (error_function "[Eval] Throw is not valid in this context.")
                         
        )
    ))

(define interpret
      (lambda (filename class_name)
        (_interpret (string->symbol class_name) (parse_body (parser filename) (empty_global_environment)))))

(define test
  (lambda (names classes results)
    ;(begin (display (car names)) (display (newline))
    (cond
      ((null? names) #t)
      ((eq? (interpret (car names) (symbol->string (car classes))) (car results)) (test (cdr names) (cdr classes) (cdr results)))
      (else (error "Failed at test " (car names) " with for " (car classes) " main, result " (interpret (car names) (symbol->string (car classes))) ". Expected " (car results))))
    ;)
    ))

(define old_test
  (lambda ()
    (test '("test1.txt" "test2.txt" "test3.txt" "test4.txt" "test5.txt" "test5.txt" "test6.txt" "test6.txt" "test7.txt" "test7.txt" "test8.txt" "test9.txt"; "test9.txt" error
                        "test10.txt" "test11.txt"); "test12.txt" "test13.txt" method overload
          '(A A A A A B A B A B B C ;B error
              Square A)
          '(10 true 30 false 30 510 30 530 105 1155 615 4321 ;error
               400 15)
          )))

(define standard_test
  (lambda ()
    (test '("test14.txt" "test15.txt" "test16.txt" "test17.txt" "test18.txt" "test19.txt" "test20.txt" "test21.txt" "test22.txt" "test23.txt" "test24.txt" "test25.txt" "test26.txt" "test27.txt" "test28.txt" )
          '(A  A   A   A  A  A  C Square List Primes A  A     A     A   A)
          '(15 12 110 125 36 54 26 117    15    17 125 100 2000400 10 18)
          )))
; Good luck with test 23. I changed the file to nprimes(3) in order to make it finish in a reasonable time