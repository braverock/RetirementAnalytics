require(DiagrammeR)

DiagrammeR::grViz("             # All instructions are within a large character string
digraph retirement_object_diagram {  # 'digraph' means 'directional graph', then the graph name

  # graph statement
  #################
  graph [layout = dot,
         rankdir = TB,
         overlap = true,
         fontsize = 10]


  # nodes (circles)
  #################
  node [shape = circle,                  # shape = circle
       fixedsize = true
       width = 1.1]                      # width of circles

  subgraph cluster_household {
    Household   [label = 'Household']
    Person     [label = 'Person A']
    PersonB     [label = 'Person B']
    Account    [label = 'Account A',
                 fontcolor = darkgreen]
    AccountB    [label = 'Account B',
                 fontcolor = red]
    AccountJWROS  [label = 'Account\nJWROS',
                   fontcolor = blue]

  }

  # edges
  {Household} -> {Person PersonB}

  Person   -> {Account AccountJWROS}

  PersonB   -> {AccountB AccountJWROS}

  # nodes (boxes)
  ###############
  node [shape = box,                     # node shape
        #fontname = Helvetica,            # text font in node
        fixedsize = false]

  subgraph cluster_accounts {
    Account
    Assets
    Taxable [label = 'Taxable:\n yes non deferred']
  }

  # cluster_accounts edges
  Account -> {Taxable Assets}

  subgraph cluster_person {
    Person
    Name
    Income
    BirthDate [label = 'Birth Date']
    Longevity [label = 'Plan Longevity\nyears']
  }

  # cluster_person edges
  Person -> {Name BirthDate Longevity Income}

  subgraph cluster_scenario {
    Scenario
    Household
    Retirement  [label = 'Target\nRetirement Date']
    Plan        [label = 'Withdrawal Plan']
    Portfolios  [label = 'Portfolio Models\nobjectives constraints rebalancing']
    Simulation
    SWR         [label = 'Safe Withdrawal Rate']
    Probability [label = 'Probability of Success']
    Reports
  }

  Scenario -> {Household Retirement Plan Portfolios} -> Simulation -> {SWR Probability Reports}

  subgraph cluster_reports{
    Reports
    Withdrawal [label='Account\nWithdrawal\nSequencing']
    Tax        [label='Tax Plan']
    MC         [label='Monte Carlo\nSimulation']
    PR         [label='Portfolio Reports']
  }

  Reports -> {Withdrawal Tax MC PR}
}
")
