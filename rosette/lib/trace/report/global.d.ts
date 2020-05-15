interface IEntry {
  type: string,
  data: any,
}

interface IStxInfo {
  stx: string,
  srcloc: ISrcloc
}

interface ISrcloc {
  line: number,
  column: number,
  source: string,
}

interface IExnTrace {
  name?: string,
  srcloc?: ISrcloc,
}

interface ICallStack {
  name: string,
  srcloc: ISrcloc,
}

interface ITraceEntrySingle {
  kind: 'no-group',
  key: string,
  exnMsg: string,
  stxInfo?: IStxInfo,

  timestamp: number,
  exnTrace: IExnTrace[],
  callStack: ICallStack[],
}

interface IGroupDetail {
  key: string,
  timestamp: number,
  exnTrace: IExnTrace[],
  callStack: ICallStack[],
  fullExnMsg: string,
}

interface ITraceEntryGroup {
  kind: 'group',
  key: string,
  exnMsg: string,
  stxInfo?: IStxInfo,

  group: IGroupDetail[],
}

type ITraceEntry = ITraceEntrySingle | ITraceEntryGroup;

interface Row<RowData> {
  row: RowData,
}